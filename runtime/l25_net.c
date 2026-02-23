#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

/* ===== TCP API ===== */

int32_t l25_net_tcp_listen(int32_t port)
{
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;

    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons((uint16_t)port);

    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(fd);
        return -1;
    }
    if (listen(fd, 128) < 0) {
        close(fd);
        return -1;
    }
    return (int32_t)fd;
}

int32_t l25_net_tcp_accept(int32_t listen_fd)
{
    struct sockaddr_in client_addr;
    socklen_t len = sizeof(client_addr);
    int fd = accept(listen_fd, (struct sockaddr*)&client_addr, &len);
    return (int32_t)fd;
}

int32_t l25_net_tcp_connect(const char* host, int32_t port)
{
    struct addrinfo hints, *res;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family   = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    char port_str[16];
    snprintf(port_str, sizeof(port_str), "%d", port);

    if (getaddrinfo(host, port_str, &hints, &res) != 0) return -1;

    int fd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
    if (fd < 0) { freeaddrinfo(res); return -1; }

    if (connect(fd, res->ai_addr, res->ai_addrlen) < 0) {
        close(fd);
        freeaddrinfo(res);
        return -1;
    }
    freeaddrinfo(res);
    return (int32_t)fd;
}

int32_t l25_net_tcp_send(int32_t fd, const char* data, int32_t len)
{
    ssize_t n = send(fd, data, (size_t)len, 0);
    return (int32_t)n;
}

char* l25_net_tcp_recv(int32_t fd, int32_t max_len, int32_t* out_len)
{
    char* buf = (char*)malloc((size_t)max_len + 1);
    if (!buf) { *out_len = 0; return (char*)malloc(1); }
    ssize_t n = recv(fd, buf, (size_t)max_len, 0);
    if (n <= 0) {
        free(buf);
        *out_len = 0;
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    buf[n] = '\0';
    *out_len = (int32_t)n;
    return buf;
}

void l25_net_close(int32_t fd)
{
    close(fd);
}

/* ===== UDP API ===== */

int32_t l25_net_udp_socket(void)
{
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    return (int32_t)fd;
}

int32_t l25_net_udp_bind(int32_t fd, int32_t port)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port        = htons((uint16_t)port);

    if (bind(fd, (struct sockaddr*)&addr, sizeof(addr)) < 0)
        return -1;
    return 0;
}

int32_t l25_net_udp_sendto(int32_t fd, const char* host, int32_t port,
                           const char* data, int32_t len)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port   = htons((uint16_t)port);
    inet_pton(AF_INET, host, &addr.sin_addr);

    ssize_t n = sendto(fd, data, (size_t)len, 0,
                       (struct sockaddr*)&addr, sizeof(addr));
    return (int32_t)n;
}

char* l25_net_udp_recvfrom(int32_t fd, int32_t max_len, int32_t* out_len)
{
    char* buf = (char*)malloc((size_t)max_len + 1);
    if (!buf) { *out_len = 0; return (char*)malloc(1); }

    struct sockaddr_in from_addr;
    socklen_t from_len = sizeof(from_addr);
    ssize_t n = recvfrom(fd, buf, (size_t)max_len, 0,
                         (struct sockaddr*)&from_addr, &from_len);
    if (n <= 0) {
        free(buf);
        *out_len = 0;
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return empty;
    }
    buf[n] = '\0';
    *out_len = (int32_t)n;
    return buf;
}
