

#include <fstream>
#include <string>

using namespace std;

int main() {
	ofstream outfile("abc", std::ofstream::out);
	int iv = 100;
	//double dv = 20.4;
	//string str = "你好";
	//outfile.write((char *)&iv, sizeof(int));
	outfile << iv << std::endl ;
	//outfile.write((char *)&dv, sizeof(double));
	//outfile.write((char *)str.data(), sizeof(char) * str.length());
}
