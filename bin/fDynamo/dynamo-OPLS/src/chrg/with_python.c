#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#define	NAME	"chrg.unix"

int chrg_server_( int *ext_code, int *natoms, int *natomsqm, 
						double *ext_atmn, double *ext_crd, 
						double *ext_ener, double *ext_qfit, 
						double *ext_grad, double *ext_hess ) {

	char				*buf;
    int 				sockfd;
	ssize_t				i, j, k, ms;
    struct sockaddr_un 	server;
	double				dv;

    if((sockfd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) {
        perror("socket generation");
        exit(1);
    }

	server.sun_family = AF_UNIX;
	strcpy( server.sun_path, NAME );

    if(connect(sockfd, (struct sockaddr*)&server, sizeof(struct sockaddr)) == -1) {
        perror("socket connection");
        exit(1);
    }

	ms = 16 + 32 * (*natoms);
	buf = malloc(ms*sizeof(char));
	memset(buf,'\0',ms);

	memcpy(&buf[0], &ms, 4);
	memcpy(&buf[4], ext_code, 4);
	memcpy(&buf[8], natoms, 4);
	memcpy(&buf[12], natomsqm, 4);
	j = 8;
	for(i=0; i<(*natoms); i++)
			memcpy(&buf[j+=8], &ext_atmn[i], 8);
	for(i=0; i<(*natoms); i++) {
			memcpy(&buf[j+=8], &ext_crd[3*i  ], 8);
			memcpy(&buf[j+=8], &ext_crd[3*i+1], 8);
			memcpy(&buf[j+=8], &ext_crd[3*i+2], 8);
		}
	send(sockfd, buf, ms, 0);
	free(buf);

	ms = 8 + 8 * (*natomsqm);
	if( *ext_code == 1 || *ext_code == 2 ) {
		ms += 24 * (*natomsqm);
		if( *ext_code == 2 ) 
			ms += 12 * (*natomsqm) * ( 3 * (*natomsqm) + 1 );
	}
	buf = malloc(ms*sizeof(char));
	memset(buf,'\0',ms);

	k = 0; do { k += recv( sockfd, buf+k, ms, 0 ); } while ( k < ms );

	memcpy(ext_ener, &buf[0], 8 );
	j = 0;
	for(i= 0;i<*natomsqm;i++) {
		memcpy(&dv,&buf[j+=8],8);
		ext_qfit[i] = dv;
	}
	if( *ext_code == 1 || *ext_code == 2 ) {
		for(i=0;i<3*(*natomsqm);i++) {
			memcpy(&dv,&buf[j+=8],8);
			ext_grad[i]= dv;
		}
		if( *ext_code == 2 ) {
			for(i=0;i<3*(*natomsqm)*(3*(*natomsqm)+1)/2;i++) {
				memcpy(&dv,&buf[j+=8],8);
				ext_hess[i] = dv;
			}
		}
	}

	free( buf );
    close(sockfd);

// debug to file to see what happens... :d
FILE *fd;
fd = fopen( "bridge.log", "at" );
fprintf(fd,"%lf\n",*ext_ener);
for(i= 0;i<*natomsqm;i++) fprintf(fd,"%8.3lf",ext_qfit[i]); fprintf(fd,"\n");
if( *ext_code == 1 || *ext_code == 2 ) {
for(i=0;i<3*(*natomsqm);i++) fprintf(fd,"%12.6lf",ext_grad[i]);
fprintf(fd,"\n");
}
if( *ext_code == 2 )
for(i=0;i<3*(*natomsqm)*(3*(*natomsqm)+1)/2;i++) fprintf(fd,"%12.6lf",ext_hess[i]);
fprintf(fd,"\n\n\n");
fflush( fd );
fclose( fd );
// --------------------------------------------------------------------------------

	return(0);
}
