#include <errno.h>
#include <ctype.h>
#include <netinet/in.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

enum { FTOP_TEST_CLIENT_BYTES = 32768, FTOP_TEST_SERVER_BYTES = 16384 };

static int tcp_fd = -1;
static int udp_fd = -1;
static int traffic_client_fd = -1;
static int traffic_listener_fd = -1;
static int traffic_server_fd = -1;

static void close_fd(int *fd) {
  if (*fd >= 0) {
    close(*fd);
    *fd = -1;
  }
}

void ftop_test_linux_network_close(void) {
  close_fd(&tcp_fd);
  close_fd(&udp_fd);
  close_fd(&traffic_client_fd);
  close_fd(&traffic_listener_fd);
  close_fd(&traffic_server_fd);
}

static int bind_loopback_socket(int type, int *port, int *sys_errno) {
  struct sockaddr_in address;
  socklen_t address_len;
  int fd;
  int reuse;

  if (port == NULL || sys_errno == NULL) return -1;
  *port = 0;
  *sys_errno = 0;

  fd = socket(AF_INET, type, 0);
  if (fd < 0) {
    *sys_errno = errno;
    return -1;
  }

  reuse = 1;
  (void)setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse));

  memset(&address, 0, sizeof(address));
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = 0;
  if (bind(fd, (struct sockaddr *)&address, sizeof(address)) != 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }
  if (type == SOCK_STREAM && listen(fd, 1) != 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }

  address_len = sizeof(address);
  if (getsockname(fd, (struct sockaddr *)&address, &address_len) != 0) {
    *sys_errno = errno;
    close(fd);
    return -1;
  }
  *port = (int)ntohs(address.sin_port);
  return fd;
}

int ftop_test_linux_network_open(int *tcp_port, int *udp_port, int *sys_errno) {
  ftop_test_linux_network_close();

  tcp_fd = bind_loopback_socket(SOCK_STREAM, tcp_port, sys_errno);
  if (tcp_fd < 0) return -1;

  udp_fd = bind_loopback_socket(SOCK_DGRAM, udp_port, sys_errno);
  if (udp_fd < 0) {
    close_fd(&tcp_fd);
    return -1;
  }
  return 0;
}

static int accept_retry(int fd, int *sys_errno) {
  int accepted_fd;

  do {
    accepted_fd = accept(fd, NULL, NULL);
  } while (accepted_fd < 0 && errno == EINTR);
  if (accepted_fd < 0 && sys_errno != NULL) *sys_errno = errno;
  return accepted_fd;
}

static int send_all(int fd, size_t byte_count, int *sys_errno) {
  char buffer[4096];
  size_t written;

  memset(buffer, 'x', sizeof(buffer));
  written = 0U;
  while (written < byte_count) {
    size_t remaining;
    ssize_t rc;

    remaining = byte_count - written;
    if (remaining > sizeof(buffer)) remaining = sizeof(buffer);
    rc = send(fd, buffer, remaining, 0);
    if (rc < 0 && errno == EINTR) continue;
    if (rc <= 0) {
      if (sys_errno != NULL) *sys_errno = rc < 0 ? errno : EIO;
      return -1;
    }
    written += (size_t)rc;
  }
  return 0;
}

static int recv_all(int fd, size_t byte_count, int *sys_errno) {
  char buffer[4096];
  size_t bytes_read;

  bytes_read = 0U;
  while (bytes_read < byte_count) {
    size_t remaining;
    ssize_t rc;

    remaining = byte_count - bytes_read;
    if (remaining > sizeof(buffer)) remaining = sizeof(buffer);
    rc = recv(fd, buffer, remaining, 0);
    if (rc < 0 && errno == EINTR) continue;
    if (rc <= 0) {
      if (sys_errno != NULL) *sys_errno = rc < 0 ? errno : EIO;
      return -1;
    }
    bytes_read += (size_t)rc;
  }
  return 0;
}

int ftop_test_linux_network_open_traffic(int *sys_errno) {
  struct sockaddr_in address;
  socklen_t address_len;
  int port;

  if (sys_errno == NULL) return -1;
  *sys_errno = 0;
  ftop_test_linux_network_close();

  traffic_listener_fd = bind_loopback_socket(SOCK_STREAM, &port, sys_errno);
  if (traffic_listener_fd < 0) return -1;

  address_len = sizeof(address);
  if (getsockname(traffic_listener_fd, (struct sockaddr *)&address, &address_len) != 0) {
    *sys_errno = errno;
    ftop_test_linux_network_close();
    return -1;
  }

  traffic_client_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (traffic_client_fd < 0) {
    *sys_errno = errno;
    ftop_test_linux_network_close();
    return -1;
  }
  if (connect(traffic_client_fd, (struct sockaddr *)&address, sizeof(address)) != 0) {
    *sys_errno = errno;
    ftop_test_linux_network_close();
    return -1;
  }

  traffic_server_fd = accept_retry(traffic_listener_fd, sys_errno);
  close_fd(&traffic_listener_fd);
  if (traffic_server_fd < 0) {
    ftop_test_linux_network_close();
    return -1;
  }

  if (send_all(traffic_client_fd, FTOP_TEST_CLIENT_BYTES, sys_errno) != 0 ||
      recv_all(traffic_server_fd, FTOP_TEST_CLIENT_BYTES, sys_errno) != 0 ||
      send_all(traffic_server_fd, FTOP_TEST_SERVER_BYTES, sys_errno) != 0 ||
      recv_all(traffic_client_fd, FTOP_TEST_SERVER_BYTES, sys_errno) != 0) {
    ftop_test_linux_network_close();
    return -1;
  }
  return 0;
}

static int safe_interface_name(const char *name) {
  size_t index;

  if (name == NULL || name[0] == '\0') return 0;
  for (index = 0U; name[index] != '\0'; ++index) {
    unsigned char ch = (unsigned char)name[index];
    if (isalnum(ch) || ch == '_' || ch == '-' || ch == '.' || ch == ':' || ch == '@') continue;
    return 0;
  }
  return 1;
}

static int read_command_output(const char *command, char *buffer, size_t capacity) {
  FILE *pipe;
  size_t bytes_read;
  size_t used;

  if (command == NULL || buffer == NULL || capacity < 2U) return -1;
  buffer[0] = '\0';
  pipe = popen(command, "r");
  if (pipe == NULL) return -1;

  used = 0U;
  while (used + 1U < capacity) {
    bytes_read = fread(buffer + used, 1U, capacity - used - 1U, pipe);
    used += bytes_read;
    if (bytes_read == 0U) break;
  }
  buffer[used] = '\0';
  return pclose(pipe) == 0 && used > 0U ? 0 : -1;
}

static const char *line_after_label(const char *text, const char *label) {
  const char *cursor;

  if (text == NULL || label == NULL) return NULL;
  cursor = strstr(text, label);
  if (cursor == NULL) return NULL;
  cursor = strchr(cursor, '\n');
  if (cursor == NULL || cursor[1] == '\0') return NULL;
  return cursor + 1;
}

static int parse_counter_after_label(const char *text, const char *label, long long *value) {
  char *end;
  const char *line;
  long long parsed;

  if (value == NULL) return -1;
  *value = 0LL;
  line = line_after_label(text, label);
  if (line == NULL) return -1;
  errno = 0;
  parsed = strtoll(line, &end, 10);
  if (end == line || errno != 0 || parsed < 0LL) return -1;
  *value = parsed;
  return 0;
}

int ftop_test_linux_ip_link_counters(
    const char *interface_name, long long *rx_bytes, long long *tx_bytes, int *sys_errno) {
  char command[160];
  char output[4096];
  int written;

  if (rx_bytes == NULL || tx_bytes == NULL || sys_errno == NULL) return -1;
  *rx_bytes = 0LL;
  *tx_bytes = 0LL;
  *sys_errno = 0;
  if (!safe_interface_name(interface_name)) {
    *sys_errno = EINVAL;
    return -1;
  }

  written = snprintf(command, sizeof(command), "ip -s link show dev %s 2>/dev/null", interface_name);
  if (written <= 0 || (size_t)written >= sizeof(command)) {
    *sys_errno = EOVERFLOW;
    return -1;
  }
  if (read_command_output(command, output, sizeof(output)) != 0 ||
      parse_counter_after_label(output, "RX:", rx_bytes) != 0 ||
      parse_counter_after_label(output, "TX:", tx_bytes) != 0) {
    *sys_errno = errno != 0 ? errno : EINVAL;
    return -1;
  }
  return 0;
}
