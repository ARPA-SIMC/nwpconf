#include <malloc.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#define NPTRS 64
#define MSIZE 1024LL*1024LL*1024LL*2LL

/*
# cat /proc/meminfo
MemTotal:       131143720 kB
MemFree:        98979476 kB
MemAvailable:   110076124 kB
...

required = memreq kB

if memreq > MemAvailable:
  siamo messi male
if MemFree < MemTotal/2 + memreq/2:
  dobbiamo pulire memoria

alloc(MIN(MemTotal/2 + mr/2, MemAvailable))
*/


void usage(char *prog) {
    printf("Usage: %s <memreq>\n", prog);
    printf("where memreq is the memory required by the user application in kB.\n");
    printf("Wipes enough memory in order to guarantee the required amount\n");
    printf("balanced among numa nodes. Two numa nodes are assumed for the moment.\n");
    exit(1);
}


void procfs_getmem(size_t *memtotal, size_t *memfree, size_t *memavailable) {
  FILE* meminfo;
  int n;
  size_t nread, len;
  char *line=NULL;
  char head[64], val[64], unit[64];

  *memtotal = -1;
  *memfree = -1;
  *memavailable = -1;

  meminfo = fopen("/proc/meminfo", "r");
  if (meminfo) {
    while ((nread = getline(&line, &len, meminfo)) != -1) {
      n = sscanf(line, "%60s %60s %60s", head, val, unit);
      if (n == 3) {
	if (!strcmp(head, "MemTotal:")) *memtotal = atoll(val);
	else if (!strcmp(head, "MemFree:")) *memfree = atoll(val);
	else if (!strcmp(head, "MemAvailable:")) *memavailable = atoll(val);
      }
    }
    free(line);
  }
  fclose(meminfo);
}      


void main(int argc, char **argv) {
  size_t memtotal, memfree, memavailable, memreq, tobefreed, margin=1024LL*1024LL*1024LL;
  char *mpt[NPTRS];
  int i, n, numanodes=2;

  if (argc < 2) usage(argv[0]);
  memreq = atoll(argv[1]);
  if (memreq <= 0) usage(argv[0]);

  procfs_getmem(&memtotal, &memfree, &memavailable);
  if (memreq > memavailable) {
    printf("memreq %zd > memavailable %zd, your application is likely to fail\n", memreq, memavailable);
    exit(2);
  }

  tobefreed = memtotal/numanodes*(numanodes-1) + memreq/numanodes;
  if (tobefreed < memfree)  {
    printf("no need to free memory\n");
    exit(0);
  }
  printf("memtotal: %zd, memfree: %zd, memavailable: %zd\n", memtotal, memfree, 
	memavailable);
  tobefreed = tobefreed > memavailable-margin ? memavailable-margin : tobefreed;
  printf("wiping: %zd kB\n", tobefreed);
  n = (int)((double)(tobefreed)*1024./(double)(MSIZE));
  n = n > NPTRS ? NPTRS : n;

  printf("allocating %d buffers of %zd bytes:\n|", n, (size_t)(MSIZE));
  for (i=0; i<n; i++) {
    if (mpt[i] = malloc(MSIZE)) memset(mpt[i], 47, MSIZE);
    if ((i+1) % 10 == 0) {printf(":");fflush(stdout);}
    else {printf(".");fflush(stdout);}
  }
  printf("|\ndeallocating:\n|");
    
  for (i=0; i<n; i++) {
    if (mpt[i]) free(mpt[i]);
    if ((i+1) % 10 == 0) {printf(":");fflush(stdout);}
    else {printf(".");fflush(stdout);}
  }
  printf("|\n");

}
