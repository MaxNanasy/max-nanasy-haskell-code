#include <stdlib.h>

#include <sys/socket.h>
#include <sys/un.h>

#include <unistd.h>

int stdout = 1, stderr = 2;

int __real___cxa_finalize ();
int __wrap___cxa_finalize ()
{
  return __real___cxa_finalize ();
}

int write (int fd, const void *buf, size_t count)
{

  int link;
  struct sockaddr_un address;

  address.sun_family = AF_UNIX;
  strcpy (address.sun_path, "/home/mnanasy/src/haskell/a.sock");

  puts (address.sun_path);

  if ((link = socket (PF_UNIX, SOCK_STREAM, 0)) == -1)
    err (EXIT_FAILURE, "socket");

  if (connect (link, (struct sockaddr *) &address, sizeof (address)) == -1)
    err (EXIT_FAILURE, "connect");

  write (link, buf, count);

  return count;

}
