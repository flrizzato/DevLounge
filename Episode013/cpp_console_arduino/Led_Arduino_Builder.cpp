// Para criar uma conexão com as portas COM1 - COM9.
// Serial* Arduino = new Serial("COM7");

// Para criar Conexão com as portas COM10 em diante;
// Serial* Arduino = new Serial("\\\\.\\COM10");

#include <iostream>
#include <fstream>
#include <Windows.h>
#include "SerialClass.h"
using namespace std;

int main()
{
	// Título do projeto
	SetConsoleTitle("Control Led Arduino - RADStudio/C++ Builder 12.1 Atenas");

	// Porta serial que vamos usuar. Caso seja diferente altere "COM4" para a encontrada.
	Serial* Porta = new Serial("COM4");


	// Comandos para Arduino.
	char Luz_ON[] = "Luz_ON";   //Envia "luz_ON" para porta serial.
	char Luz_OFF[] = "Luz_OFF"; //Envia "Luz_Off" para a porta serial.
	char leitura[50] = "\0"; //Armazena dados da entrada da porta Serial.

	int opc; // Guarda opção 1 ou 2;

	while (Porta->IsConnected())
	{
		cout << endl; // Retorno.
        cout << "--------------------------------------------------------" <<endl;
        cout << "----- RADStudio/ C++ Builder 12.1 Console Terminal -----" <<endl;
        cout << "--------------------  ATENAS ---------------------------" <<endl;
        cout << "" <<endl;
        cout << "" <<endl;
        cout << "Selecione: " << endl;
        cout << " " <<endl;
        cout << " " <<endl;
		cout << "   1 - Digite 1 para acender Led"  << endl << endl; // texto.
        cout << "   2 - Digite 1 para apagar  Led"  << endl << endl; // texto.
        cout << "   " << endl << endl; // texto.

		cin >> opc; // Aguardando decisão;

		switch (opc) // Espera receber 1 ou 2.
		{
		case 1:
			// Acender Luz
			cout << "Enviando: " << Luz_ON << endl; // Mostra texto no terminal.
			Porta->WriteData(Luz_ON, sizeof(Luz_ON) - 1); //Envia a porta o texto "Luz_ON".
			break;

		case 2:
			// Apagar luz.
			cout << "Enviando: " << Luz_OFF << endl;
			Porta->WriteData(Luz_OFF, sizeof(Luz_OFF) - 1);
			break;

		default: // Se digitar outro numero que não seja o 1 ou 2;
			cout << "Digite apenas opção 1 ou 2."; // Mensagem alerta
		}


		Sleep(500);
		int n = Porta->ReadData(leitura, 49); // Recebe dados da porta serial
		if (n > 0)
		{
			leitura[n + 1] = '\0'; // Limpa variável
			cout << "Recibido: " << leitura << endl; //Mostra valor recebido.
			cout << "-------------------" << endl;
		}

		cin.ignore(256, '\n'); // Limpar buffer do teclado.
	}
}

