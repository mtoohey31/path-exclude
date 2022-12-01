#include "idris_few.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

/*
  NOTE: this forbidden technique of setting the args individually is necessary
  because Idris2 currently only supports ffi for the types listed here:
  https://github.com/idris-lang/Idris2/blob/1875f622486249e831850f5598ee30a3114a61c0/docs/source/ffi/ffi.rst#primitive-ffi-types
*/

int _argc;
char** _argv;

void idris2_setFewArgc(int argc) {
  _argc = argc;
  _argv = malloc(sizeof(char*) * (argc + 1));
  _argv[_argc] = (char*) 0;
}

void idris2_setFewArgn(int n, char* argn) {
  _argv[n] = argn;
}

int idris2_few() {
  int pid = fork();
  if (pid < 0) {
    // a failure occurred, should be reported, and might be checkable via errno
    return -2;
  } else if (pid == 0) {
    // the child

    // execute the command, we have to error here instead of passing it back
    // because there's no way for us to communicate to the parent in a way that
    // a child behaving as expected can't
    if (execvp(_argv[0], _argv) < 0) {
      perror("px: execvp");
    }

    exit(1);
  } else {
    // the parent

    // wait for the child
    int status;
    waitpid(pid, &status, 0);

    // clean up
    free(_argv);
    _argc = 0;

    if (WIFEXITED(status)) {
      // we can return the exit code
      return WEXITSTATUS(status);
    } else {
      // some other kind of failure, maybe the child was signalled, won't be
      // checkable via errno
      return -1;
    }
  }
}
