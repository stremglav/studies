#include <iostream> 
#include <string> 
//using namespace std;

int main() { 
	char* hashtable[10]; 
	std::string aaa = "qwertyuiop";
	std::cout<<aaa<<std::endl;

	int i=1; hashtable[i]=new char[strlen("imin")];     
	strcpy(hashtable[i],"imin"); 
	std::cout<<"["<<hashtable[i]<<"]"<<std::endl; 
	int j=2; 
	hashtable[j]=new char[strlen("imax")];     
	strcpy(hashtable[j],"imax");     
	std::cout<<"["<<hashtable[i]<<"]  ["<<hashtable[j]<<"]"<<std::endl;     
	return 0;
}
