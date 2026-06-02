#include <errno.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static int tcp_fd = -1;
static int udp_fd = -1;

static void close_fd(int *fd) {
  if (*fd >= 0) {
    close(*fd);
    *fd = -1;
  }
}

void ftop_test_linux_network_close(void) {
  close_fd(&tcp_fd);
  close_fd(&udp_fd);
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
