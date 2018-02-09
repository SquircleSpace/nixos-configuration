#include <stdlib.h>
#include <stdio.h>
#include <spawn.h>
#include <unistd.h>
#include <sys/wait.h>
#include <string.h>

int markWindows() {
    pid_t pid = 0;
    const char *efiBootmgrArgs[] = {"efibootmgr", "-n", "2", NULL};
    int status = posix_spawnp(&pid, "efibootmgr", NULL, NULL, (char * const *)efiBootmgrArgs, environ);
    if (0 != status) {
	printf("Couldn't fork efibootmgr. Got status %d\n", status);
	return status;
    }

    pid_t waitedPid = wait(&status);
    if (0 != status || waitedPid != pid) {
	printf("efibootmgr failed. Got status %d\n", status);
	return status;
    }

    return 0;
}

int bootWindows() {
    int status = markWindows();
    if (0 != status) {
	return status;
    }

    pid_t pid = 0;
    const char *rebootArgs[] = {"reboot", NULL};
    status = posix_spawnp(&pid, "reboot", NULL, NULL, (char * const *)rebootArgs, environ);
    if (0 != status) {
	printf("Couldn't fork reboot. Got status %d\n", status);
	printf("%s\n", strerror(status));
	return status;
    }

    pid_t waitedPid = wait(&status);
    if (0 != status || waitedPid != pid) {
	printf("reboot failed. Got status %d\n", status);
	return status;
    }

    printf("done\n");
    return 0;
}

int removeBootNext() {
    pid_t pid = 0;
    const char *efiBootmgrArgs[] = {"efibootmgr", "-N", NULL};
    int status = posix_spawnp(&pid, "efibootmgr", NULL, NULL, (char * const *)efiBootmgrArgs, environ);
    if (0 != status) {
	printf("Couldn't fork efibootmgr. Got status %d\n", status);
	return status;
    }

    pid_t waitedPid = wait(&status);
    if (0 != status || waitedPid != pid) {
	printf("efibootmgr failed. Got status %d\n", status);
	return status;
    }

    return 0;
}

int printUsage(char *name) {
    printf("usage: %s [--now | --later | --cancel]\n", name);
    return 2;
}

int main(int argc, char **argv) {
    if (argc < 2) {
	return printUsage(argv[0]);
    }

    if (0 == strcmp("--now", argv[1])) {
	return bootWindows();
    } else if (0 == strcmp("--later", argv[1])) {
	return markWindows();
    } else if (0 == strcmp("--cancel", argv[1])) {
	return removeBootNext();
    } else {
	return printUsage(argv[0]);
    }
}
