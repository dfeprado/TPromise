# TPromise
## O conceito _promise_ do ES no DELPHI

## Nova versão! 
### Agora mais simples!
Apenas duas units

 - **promise.pas**: define os tipos do TPromise - interfaces e procedures
 - **promise.concrete.pas**: define a classe TPromise

## Era uma vez... eventos pra lá e pra cá (arg!)
Imagine que temos uma função que levará muito tempo para ser executada:
```
function facaAlgo: string;
begin
	sleep(10000); // imagine um processo longo aqui (10s)
	result := 'Deu certo!';
end
``` 
Essa função pode ser colocada dentro de um **IFuture\<string\>** para que possa rodar em paralelo:
```
function facaAlgo: string;
begin
	xFuture = TTask.Future<string>(
		function: string
		begin
			sleep(10000); // imagine um processo longo aqui
			result := 'Deu certo!';
		end
	);
end
``` 
Mas e agora, como esperar pelo **IFuture**? Se usar o método _Wait()_, a thread principal ficará travada. Uma solução é colocar ele dentro de uma **TTask** ou **TThread** e deixar que elas invoquem um evento da thread principal:
```
function facaAlgoAssincrono: string;
begin
	xFuture = TTask.Future<string>(
		function: string
		begin
			sleep(10000); // imagine um processo longo aqui
			result := 'Deu certo!';
		end
	);

	TTask.Run(
		procedure
		begin
			xResultado := xFuture.Value; // bloqueia aqui até ter o resultado
			TThread.Queue(
				nil,
				procedure
				begin
					self.OnFacaAlgoFinished(xResultado); // invocando o evento!
				end;
			);
		end
	);
end
```
E pronto, **facaAlgo()** agora é assíncrona! Mas, há um problema nessa abordagem: o uso de _eventos_.

Imagina um sistema com um componente de comunicação que faz uma requisição por uma **procedure** mas recebe a resposta por um evento:
```
procedure pesquisar(const pProduto: string);
begin
	ObjComunicacao.send('/pesquisa/por/produto/', pProduto);
end;

// evento onde os dados do ObjComunicacao chegam do servidor
procedure OnObjComunicacaoPublishReceived(ACabecalho, AResposta: string);
begin
	// olha que eu nem coloquei um ID. Imagine como isso ficaria
	// para várias requisições nesse cabeçalho, por threads diferentes!
	if (ACabecalho = '/pesquisa/por/produto/') then
	begin
		facaAlgoComAResposta(AResposta);
	end
end

...

// em um lugar qualquer do código:
pesquisar('maçãs');	
queroFazerAlgoComARespostaAqui() // Isso não é possível, pois vou
								// receber a resposta por um EVENTO!!!
``` 
Essa abordagem é comum no DELPHI, e o problema disso é: fluxo de código quebrado, dificuldade para depuração(se não acredita, veja um projeto gigante; imagine como seria a verificação para dezenas/centenas de cabeçalhos e como seria o acoplamento para poder executar o **facaAlgoComAResposta()** de cada unit que pesquisou um produto).
O problema de usar eventos para esse tipo de procedimento é que eles são **eventos**!

Eventos são coisas como um clique no mouse; um pressionar de um botão; um timer estourando seu intervalo; um envio esporádico do servidor para o ObjComunicacao. Porém, quando um procedimento que se parece uma _requisição_ (eu peço algo e espero uma resposta) fica refém de um evento, temos um problema de descentralização de código, quebra de fluxo e [ALTAMENTE POSSIVELMENTE] acoplamento.

Uma forma de resolver o acoplamento seria usando o padrão _observer_. Isso resolve bem o problema de acoplamento, mas ainda não resolve muito bem a descentralização do código e a quebra de fluxo (mesmo que, provavalmente com o _observer_, as coisas estejam mais juntas).

_Então, como resolver esses problemas?_

### Sras e srs, apresento-lhes a PROMISE!
#### conceito extremamente útil _inspirado_  na PROMISE do Javascript.
Veja como fica o código anterior se usarmos uma promise:
##### Sem promise
```
function facaAlgoAssincrono: string;
begin
	xFuture = TTask.Future<string>(
		function: string
		begin
			sleep(10000); // imagine um processo longo aqui
			result := 'Deu certo!';
		end
	);

	TTask.Run(
		procedure
		begin
			xResultado := xFuture.Value; // bloqueia aqui até ter o resultado
			TThread.Queue(
				nil,
				procedure
				begin
					self.OnFacaAlgoFinished(xResultado); // invocando o evento!
				end;
			);
		end
	);
end
```
##### Com promise
```
function facaAlgo: string;
begin
	TPromise<String, Exception>.New(
		procedure (Resolve: TPromiseResolveProcedure<String>, Reject: TPromiseRejectProcedure<Exception>) 
		begin
			try
				sleep(10000); // imagine um processo longo aqui
				Resolve('Deu certo!');
			except
				// Não temos interesse nesse tipo de excessão
				if ExceptObjet is EOperationCancelled then
					Exit();

				// Para os demais...
				Reject(AcquireExceptionObject() as Exception)
			end;
		end
	).&Then(
		procedure(const Value: String) 
		begin
			Self.OnFacaAlgoFinished(Value);
		end
	).Catch(
		procedure(const Error: Exception)
		begin
			ShowMessage(Error.Message);
			Error.Free();
		end
	)
end
```
Se isso não é bonito, eu não sei o que é!

Agora, imagine que o nosso objeto de comunicação (ObjComunicacao) retornasse uma promise quando o método pesquisar fosse invocado:
##### Sem promise
```
procedure pesquisar(const pProduto: string);
begin
	ObjComunicacao.send('/pesquisa/por/produto/', pProduto);
end;

// evento onde os dados do ObjComunicacao chegam do servidor
procedure OnObjComunicacaoPublishReceived(ACabecalho, AResposta: string);
begin
	if (ACabecalho = '/pesquisa/por/produto/') then
	begin
		facaAlgoComAResposta(AResposta);
	end
end

...

// em um lugar qualquer do código:
pesquisar('maçãs');	
queroFazerAlgoComARespostaAqui() // Isso não é possível, pois vou
								// receber a resposta por um EVENTO!!!
``` 
##### Com promise
```
function pesquisar(const pProduto: string): IPromise<String, String>;
begin
	// esse camarada aqui retorna uma IPromise<string> ;)
	result := ObjComunicacao.send('/pesquisa/por/produto/', pProduto);
end;

...

// em um lugar qualquer do código:
pesquisar('maçãs')
	.&Then(
		procedure (const pResposta: string)
		begin
			facaAlgoComAResposta(pResposta);
		end;
	); //agora eu posso fazer algo com a resposta aqui! :))
``` 
Não é lindo?! Código centralizado, desacoplado e de fácil depuração!