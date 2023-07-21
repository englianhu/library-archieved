#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <conio.h>
#include <iostream.h>
#define  N   25000     // ������Ԫ�صĸ���

void insertsort(int R[N+1])  // ֱ�Ӳ�������
{
	int i,j;
	for (i=2; i<=N; i++) {
		R[0]=R[i];    // ���ü�����
		j=i-1;
		while (R[0]<R[j]) {
			R[j+1]=R[j];
			j--;
		}
		R[j+1]=R[0];
	}
}

void shellsort(int R[N+1])  // ϣ������
{
	int i,j,gap;
	int x;
	gap=N/2;   // ���ó�ʼ����
	while (gap>0) {
		for (i=gap+1; i<=N; i++) {
			j=i-gap;
			while (j>0) 
				if(R[j]>R[j+gap]) {
					x=R[j];
					R[j]=R[j+gap];
					R[j+gap]=x;
					j=j-gap;
				}
				else 
					j=0;
		}
		gap=gap/2;   // ��С����
	}
}

void bubblesort(int R[N+1])  // ��������
{
	int i,j,noswap;
	int temp;
	for (i=1; i<=N-1; i++) {
		noswap=1;
		for (j=N; j>=i+1; j--) 
			if(R[j]<R[j-1]) {
				temp=R[j];
				R[j]=R[j-1];
				R[j-1]=temp;
				noswap=0;
			}
		if(noswap)
			break;
	}
}

int partition(int R[N+1],int low,int high)  // ����������Ӻ�����ȡ������Ԫ�أ�
{
	int i,j;
	i=low;
	j=high;
	R[0]=R[low];   // ȡ������Ԫ��
	do {  // �ӱ�����˽�������м�ɨ��
		while ((j>i) && (R[j]>=R[0]))
			j--;
		if(i<j) {
			R[i]=R[j];
			i++;
		}
		while ((i<j) && (R[i]<=R[0]))
			i++;
		if(i<j) {
			R[j]=R[i];
			j--;
		}
	} while (i<j);
	R[i]=R[0];  // ����Ԫ�ص�λ
	return i;   // ��������λ��
}

void quicksort(int R[N+1],int low,int high)  // ��������
{
	int i;
	if(low<high) {
		i=partition(R,low,high);  // ����Rһ��Ϊ��
		quicksort(R,low,i-1);     // �Ե��ӱ�ݹ�����
		quicksort(R,i+1,high);    // �Ը��ӱ�ݹ�����
	}
}

void selectsort(int R[N+1])    // ֱ��ѡ������
{
	int i,j,k;
	int temp;
	for (i=1; i<=N-1; i++) {
		k=i;
		for (j=i+1; j<=N; j++)
			if(R[j]<R[k])
				k=j;   // ��kָ��ÿ������������ε���СԪ��
			if(k!=i) {
				temp=R[i];
				R[i]=R[k];
				R[k]=temp;
			}
	}
}

void sift(int R[N+1],int s,int m)  // ��������Ӻ�����ɸѡ�㷨��ʹR[s..m]��Ϊһ������ѣ�
{
	int i,j;
	int temp;
	temp=R[s];
	i=s; 
	j=2*i;   // R[j]��R[i]������
	while (j<=m) {
		if((j<m) && (R[j]<R[j+1]))
			j++;  // ���Һ��ӽϴ����j�޸�Ϊ�Һ��ӵ��±�
		if(temp<R[j]) {
			R[i]=R[j];  // ��R[j]�������׵�λ����
			i=j;
			j=2*i;      // �޸�i��j��ֵ���Ա��������ɸѡ
		}
		else 
			break;      // ɸѡ��ɣ���ֹѭ��
	}
	R[i]=temp;          // ��ɸ����ֵ��������λ��
}

void heapsort(int R[N+1])  // ������
{
	int i;
	int temp;
	for (i=N/2; i>=1; i--)
		sift(R,i,N);       // ������ʼ��
	for (i=N; i>=2; i--) { // ����N-1��ѭ������ɶ�����
		temp=R[1];
		R[1]=R[i];
		R[i]=temp;         // ����һ��Ԫ��ͬ��ǰ���������һ��Ԫ�ضԻ�
		sift(R,1,i-1);     // ɸѡR[1]��㣬�õ�(N-1)�����Ķ�
	}
}

void main()
{
	int R[N+1],RR[N+1];     // �������Ԫ����
	clock_t start,finish;   // ���ں������еļ�ʱ
	double duration;
	int i;

	cout<<"The initial data: "<<endl;
	for (i=1; i<=N; i++) {
		R[i]=rand()%5001;
		RR[i]=R[i];
		cout<<R[i]<<"   ";
		if(i%10==0)
			cout<<endl;
	}

///////////////////////////////////////////////////////////
	getchar();        // ֱ�Ӳ�������
	start = clock();  // ��ʱ��ʼ
	insertsort(RR);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the insert sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////
    getchar();        // ϣ������
	for (i=1; i<=N; i++) {
		RR[i]=R[i];
	}
	start = clock();  // ��ʱ��ʼ
	shellsort(RR);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the shell sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////
    getchar();        // ��������
	for (i=1; i<=N; i++) {
		RR[i]=R[i];
	}
	start = clock();  // ��ʱ��ʼ
	bubblesort(RR);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the shell sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////
    getchar();        //  ��������
	for (i=1; i<=N; i++) {
		RR[i]=R[i];
	}
	start = clock();  // ��ʱ��ʼ
	quicksort(RR,1,N);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the shell sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////
    getchar();        //  ֱ��ѡ������
	for (i=1; i<=N; i++) {
		RR[i]=R[i];
	}
	start = clock();  // ��ʱ��ʼ
	selectsort(RR);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the shell sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////
    getchar();        //  ������
	for (i=1; i<=N; i++) {
		RR[i]=R[i];
	}
	start = clock();  // ��ʱ��ʼ
	heapsort(RR);
	finish = clock();   // ��ʱ����
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout<<"The result based on the shell sort is :"<<endl;
	for (i=1; i<=N; i++) {
		cout<<RR[i]<<"    ";
		if(i%10==0)
			cout<<endl;
	}
	cout<<"The Run Time is: "<<duration<<" seconds"<<endl;

///////////////////////////////////////////////////////////

}






