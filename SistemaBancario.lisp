;;Aluna: Irvna Maria Costa Soares - RGM: 49115
;;Para Carregar: (load "Diretório da pasta que o arquivo esta localizado")
;;Exemplo: (load "C:/Users/irvna/OneDrive/Área de Trabalho/UEMS/PP/LISP/TrabalhoLisp.lisp")
;;Para executar: (logar)
;;Para utilizar: Escolha as opções que estão disponiveis no menu através de sua numeração.
;;Relatórios: Os relatórios são salvo diretamente na pasta de instalação do clisp com o nome composto pelo tipo do relatorio e o CPF do cliente
(defstruct aplicacoes
    tipo
    valor-inicial
    rendimento
    data-inicial-investimento
    prazo-final
    valor-atual
)

(defstruct conta
    agencia 
    numero
    data-abertura
    data-encerramento
    estado
    saldo
    historico
    aplicacoes
    historico-transferencias
    historico-pagamentos
    historico-aplicacoes
    historico-saque
    historico-deposito
)

(defstruct cliente
    nome
    sobrenome
    cpf
    sexo
    data-nascimento
    endereco
    conta
)
  
(defvar *clientes* '())

;;escrita dos menus
(defun menu-principal ()
    (format t "~%~%----------MENU INICIAL----------~%")
    (format t "1 - Gerenciamento de Clientes e Conta~%")
    (format t "2 - Gerenciamento de Aplicações Financeiras~%")
    (format t "3 - Transferências entre Contas~%")
    (format t "4 - Pagamentos~%")
    (format t "5 - Saques e Depósitos~%")
    (format t "6 - Relatórios Gerenciais~%")
    (format t "7 - Sair~%")
    (format t "Escolha uma opção: ")
)

(defun menu-gerenciamento-cliente ()
    (format t "~%~%----------MENU DE GERENCIAMENTO DE CLIENTES E CONTA----------~%")
    (format t "1 - Abrir Cadastro e Conta~%")
    (format t "2 - Atualizar Cadastro do Cliente~%")
    (format t "3 - Buscar Cliente por CPF~%")
    (format t "4 - Consultar Saldo na Conta~%")
    (format t "5 - Consultar Histórico da Conta~%")
    (format t "6 - Encerrar Conta~%")
    (format t "7 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
) 

(defun menu-gerenciamento-aplicacoes ()
    (format t "~%~%----------MENU DE GERENCIAMENTO DE APLICAÇÕES FINANCEIRAS----------~%")
    (format t "1 - Abrir Poupança~%")
    (format t "2 - Abrir Título do Governo~%")
    (format t "3 - Abrir Fundo de Investimento~%")
    (format t "4 - Consultar Rendimentos~%")
    (format t "5 - Resgatar Rendimentos~%")
    (format t "6 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
)

(defun menu-gerenciamento-transferencias ()
    (format t "~%~%----------MENU DE GERENCIAMENTO DE TRANSFERÊNCIAS----------~%")
    (format t "1 - Transferência entre Contas~%")
    (format t "2 - Histórico de Tranferências~%")
    (format t "3 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
)

(defun menu-gerenciamento-pagamentos ()
    (format t "~%~%----------MENU DE PAGAMENTOS----------~%")
    (format t "1 - Realizar Pagamento~%")
    (format t "2 - Histórico de Pagamentos~%")
    (format t "3 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
)

(defun menu-gerenciamento-saque-deposito ()
    (format t "~%~%----------MENU DE SAQUES E DEPÓSITOS----------~%")
    (format t "1 - Realizar Saque~%")
    (format t "2 - Histórico de Saques~%")
    (format t "3 - Realizar Depósito~%")
    (format t "4 - Histórico de Depósito~%")
    (format t "5 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
)

(defun menu-gerenciamento-relatorios-gerais ()
    (format t "~%~%----------MENU DE RELATÓRIOS GERAIS----------~%")
    (format t "1 - Gerar Relatório de Transferência~%")
    (format t "2 - Gerar Relatório de Pagamento~%")
    (format t "3 - Gerar Relatório de Aplicações Financeiras~%")
    (format t "4 - Gerar Relatório de Desempenho da Agência~%")
    (format t "5 - Voltar para Menu Inicial~%")
    (format t "Escolha uma opção: ")
);;;fim da escrita de menus

;;esta função verifica se o CPF digitado ja existe no banco de dados
(defun verifica-existencia-cpf (cpf-veri)
    (find-if (lambda (cliente)
        (equal (cliente-cpf cliente) cpf-veri)) *clientes*
    )
)

;;esta função verifica se o numero da conta existe ou não
(defun verifica-numero (num)
    (find-if (lambda (cliente)
        (when (cliente-conta cliente)
            (equal (conta-numero (cliente-conta cliente)) num)
        ))
        *clientes*
    )
)

;;chama a função de gera um numero de 0 a 500 e verificar a existencia do numero, caso não exista salva o numero
(defun gerar-numero ()
    (let ((num (random 500)))
        (if (verifica-numero num)
            (gerar-numero) 
            num
        )
    )
)

;;escreve todos os dados necessários dos clientes
(defun escrita-dados-cliente (cliente)
    (format t "~%Dados do Cliente:
                Nome: ~a 
                Sobrenome: ~a
                CPF: ~a
                Sexo: ~a
                Data de Nascimento: ~a
                Endereço: ~a
                Agência: ~a
                Numero: ~a
                Estado: ~a
                Data Abertura: ~a~%"
            (cliente-nome cliente)
            (cliente-sobrenome cliente)
            (cliente-cpf cliente)
            (cliente-sexo cliente)
            (cliente-data-nascimento cliente)
            (cliente-endereco cliente)
            (conta-agencia (cliente-conta cliente))
            (conta-numero (cliente-conta cliente))
            (conta-estado (cliente-conta cliente))
            (conta-data-abertura (cliente-conta cliente))
    )
)

;;escreve todos os dados necessarios da conta
(defun escrita-dados-conta (cliente)
    (format t "~%Dados da conta:
                Nome: ~a ~a
                CPF: ~a
                Agência: ~a
                Numero: ~a
                Estado: ~a
                Saldo: ~,2f~%"
            (cliente-nome cliente)
            (cliente-sobrenome cliente)
            (cliente-cpf cliente)
            (conta-agencia (cliente-conta cliente))
            (conta-numero (cliente-conta cliente))
            (conta-estado (cliente-conta cliente))
            (conta-saldo (cliente-conta cliente))
    )
)

;;escreve todos os dados do historico da conta
(defun escrita-historico-conta (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a~%"
                (cliente-nome cliente)
    )
    (dolist (ocorrencia (conta-historico (cliente-conta cliente)))
        ;;separa a lista em 2, pois a primeira é a data e a segunda ao motivo
        (let* ((data (first ocorrencia)) 
                (motivo (second ocorrencia))
            )  
            (format t "     Data: ~a/~a/~a - Motivo: ~a~%"
                (first data) (second data) (third data) motivo
            )
        )
    )
)

;;escreve os rendimentos obtidos a partir de uma aplicação
(defun escrita-rendimento (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a~%"
                (cliente-nome cliente)
    )
    (dolist (ocorrencia (conta-aplicacoes (cliente-conta cliente)))
        (format t "                Tipo: ~a
                    Valor Investido: ~a
                    Rendimento: ~a
                    Data Inicial: ~a
                    Data Final: ~a
                    Rendimento Gerado: ~,2f~%~%"
                (aplicacoes-tipo ocorrencia)
                (aplicacoes-valor-inicial ocorrencia)
                (aplicacoes-rendimento ocorrencia)
                (aplicacoes-data-inicial-investimento ocorrencia)
                (aplicacoes-prazo-final ocorrencia)
                (aplicacoes-valor-atual ocorrencia)
        )
    )
)

;;verifica a dara atual do computador
(defun data-timestamp (timestamp)
    ;;converte o timestamp, que recebe a data atual do comptador, e modifica para uma data legivel
    (multiple-value-bind (second minute hour day month year dow dst tz)
        (decode-universal-time timestamp)
        ;;retorna uma lista de dia, meses e ano
        (list day month year)
    )
)

;;adiciona 1 ocorrencia de movimentacao no historico do cliente,, dizendo o motivo
(defun adicionar-ocorrencia-historico (cliente motivo)
    (let* ((historico-atual (conta-historico (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-historico (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-historico historico-atual)
        (setf (conta-historico (cliente-conta cliente)) historico-atual)
    )
)

(defun cadastrar-novo-cliente ()
    (let ((nome nil)
            (sobrenome nil)
            (cpf nil)
            (sexo nil)
            (data-nascimento nil)
            (endereco nil)
            (agencia nil)
            (numero nil)
            (data-abertura nil)
            (estado nil)
            (saldo nil)
            (historico nil)
          )
    )

    (format t "~%Cadastrar Cliente:~%")
    (format t "~%Nome: ")
    (setf nome (read-line))
    (format t "~%Sobrenome: ")
    (setf sobrenome (read-line))
    (format t "~%CPF: ")
    (setf cpf (read-line))
    (format t "~%Sexo: ")
    (setf sexo (read-line))
    (format t "~%Data de Nascimento (XX/XX/XXXX): ")
    (setf data-nascimento (read-line))
    (format t "~%Endereço: ")
    (setf endereco (read-line))

    (format t "~%Agência: ")
    (setf agencia (read-line))

    (setf data-hoje (get-universal-time))
    (setf data-abertura (data-timestamp data-hoje))

    (setf estado "Aberta")
    (setf numero (gerar-numero))
    (setf saldo 0)

    (if (verifica-existencia-cpf cpf)
        (format t "~%CPF já encontrado.~%")
        (progn
            ;;inicializa o historico de pagamentos, transferencias e historico da conta
            (setf historico-pagamentos nil)
            (setf historico-transferencias nil)
            (setf historico-saque nil)
            (setf historico-deposito nil)
            (setf historico-aplicacoes nil)
            (setf historico (list (list data-abertura "Conta Aberta!")))
            (let((cria-conta (make-conta :agencia agencia
                                        :data-abertura data-abertura
                                        :estado estado
                                        :numero numero
                                        :saldo saldo
                                        :historico historico
                                        :historico-transferencias historico-transferencias
                                        :historico-pagamentos historico-pagamentos
                                        :historico-saque historico-saque
                                        :historico-deposito historico-deposito
            )))
            (push (make-cliente :nome nome
                                :sobrenome sobrenome
                                :cpf cpf
                                :sexo sexo
                                :data-nascimento data-nascimento
                                :endereco endereco
                                :conta cria-conta
            ) *clientes*))
            (format t "~%Cliente e Conta cadastrado com sucesso!")
        )
    ) 
)

(defun atualizar-dados-cliente ()
    (format t "~%Digite o CPF que deseja atualizar: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                        (format t "~%Qual campo você deseja modificar?~%")
                        (format t "1 - Nome~%")
                        (format t "2 - Sobrenome~%")
                        (format t "3 - CPF~%")
                        (format t "4 - Sexo~%")
                        (format t "5 - Data de Nascimento~%")
                        (format t "6 - Endereço~%")
                        (format t "7 - Retornar ao menu anterior~%")
                        (format t "Escolha uma opção:")

                        (let ((escolha (read)))
                            (case escolha
                                (1 (format t "~%Digite o nome modificado: ")
                                    (setf (cliente-nome busca-cliente) (read-line))
                                    (adicionar-ocorrencia-historico busca-cliente "Nome Alterado")
                                    (format t "~%Nome alterado com sucesso!~%"))
                                (2 (format t "~%Digite o sobrenome modificado: ")
                                    (setf (cliente-sobrenome busca-cliente) (read-line))
                                    (adicionar-ocorrencia-historico busca-cliente "Sobrenome Alterado") 
                                    (format t "~%Sobrenome alterado com sucesso!~%"))
                                (3 (format t "~%Digite o CPF modificado: ")
                                    (setf (cliente-cpf busca-cliente) (read-line)) 
                                    (adicionar-ocorrencia-historico busca-cliente "CPF Alterado")
                                    (format t "~%CPF alterado com sucesso!~%"))
                                (4 (format t "~%Digite o sexo modificado: ")
                                    (setf (cliente-sexo busca-cliente) (read-line)) 
                                    (adicionar-ocorrencia-historico busca-cliente "Sexo Alterado")
                                    (format t "~%Sexo alterado com sucesso!~%"))
                                (5 (format t "~%Digite o data de nascimento modificada (XX/XX/XXXX): ")
                                    (setf (cliente-data-nascimento busca-cliente) (read-line))
                                    (adicionar-ocorrencia-historico busca-cliente "Data de Nascimento Alterado")
                                    (format t "~%Data de Nascimento alterado com sucesso!~%"))
                                (6 (format t "~%Digite o endereço modificado: ")
                                    (setf (cliente-endereco busca-cliente) (read-line))
                                    (adicionar-ocorrencia-historico busca-cliente "Endereço Alterado")
                                    (format t "~%Endereço alterado com sucesso!~%"))
                                (7 (gerenciamento-clientes-contas))
                                (otherwise (format t "~%Escolha não existe. Tente novamente!~%") (atualizar-dados-cliente))
                            )
                        )
                )
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

(defun buscar-cadastro-cliente ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (escrita-dados-cliente busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;esta funcao consulta o saldo da conta do cliente equivalente do cof digitado
(defun consultar-saldo-conta ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    ;;escreve os dados da conta e seu saldo
                    (escrita-dados-conta busca-cliente)
                    (adicionar-ocorrencia-historico busca-cliente "Saldo da conta foi consultado")
                )
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;esta funcao consulta o historico caso o cpf digitado exista
(defun consultar-historico-conta ()
    ;;verifica a existencia do CPF digitado
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                ;;se o CPF existe o historico do cliente buscado é escrito
                (escrita-historico-conta busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;esta funcao encerra a conta do cliente
(defun encerrar-conta ()
    ;;verifica a existencia do CPF digitado
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    ;;caso o CPF seja valido ele ira encerrar a conta
                    (let ((data (data-timestamp (get-universal-time))))
                        (setf (conta-estado (cliente-conta busca-cliente)) "Encerrada")
                        (setf (conta-data-encerramento (cliente-conta busca-cliente)) data)
                        ;;adiciona ao historico que a conta foi encerrada
                        (adicionar-ocorrencia-historico busca-cliente "Conta Encerrada!")
                        (format t "Conta Encerrada com sucesso!")
                    )
                )
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;retira o valor passado como argumento da conta do cliente informado
(defun retirar-valor-saldo (valor cliente)
    ;;Retira o valor recebido da conta
    (setf (conta-saldo (cliente-conta cliente)) (- (conta-saldo (cliente-conta cliente)) valor))
    ;;Adiciona no historico do cliente que o valor foi retirado
    (let ((motivo (format nil "Valor ~a foi retirado da conta" valor)))
        (adicionar-ocorrencia-historico cliente motivo)
    )
)

;;deposita o valor passado como argumento da conta do cliente informado
(defun depositar-valor-saldo (valor cliente)
    ;;Deposita o valor recebido da conta
    (setf (conta-saldo (cliente-conta cliente)) (+ (conta-saldo (cliente-conta cliente)) valor))
    ;;Adiciona no historico do cliente que o valor foi retirado
    (let ((motivo (format nil "Valor ~a foi depositado na conta" valor)))
        (adicionar-ocorrencia-historico cliente motivo)
    )
)

;;transforma a data recebida como parametro em uma lista de 3 argumentos
(defun transformar-data (data)
    ;;Transforma a data em uma lista
    (let* ((dia (parse-integer (subseq data 0 2)))
            (mes (parse-integer (subseq data 3 5)))
            (ano (parse-integer (subseq data 6 10))))
        (list dia mes ano)
    )
)

;;calcula a diferença de meses da data inicial até a data final da aplicação realizada
(defun calcular-diferenca-meses (data-inicial data-final)
    ;; Calcula o número de meses completos entre data-inicial e data-final
    (let* ((diferenca-meses (- (second data-final) (second data-inicial)))
            (diferenca-anos (- (third data-final) (third data-inicial)))
            (meses (+ (* diferenca-anos 12) diferenca-meses))
        )
        ;;retorna os meses
        meses
    )
)

;;calcula o rendimento que a aplicação teve a partir da data inicial, data final, valor inicial e o seu rendimento
(defun calcular-rendimento-aplicacoes (rendimento valor-inicial data-inicial data-final) 
    (let* ((meses (calcular-diferenca-meses data-inicial data-final))
            ;; Calcula o valor final da aplicação após o rendimento
            (valor-final (* valor-inicial (expt (+ 1 rendimento) meses)))
            )
        ;; Retorna as informações e o valor final da aplicação
        (format t "~%Confirmando informações recebidas: ")
        (format t "~%Data Inicial: ~a" data-inicial)
        (format t "~%Data Final: ~a" data-final)
        (format t "~%Meses: ~a~%" meses)
        valor-final
    )
)

;;abre uma poupança para o cliente identificado pelo CPF com seu rendimento de 0,01
;;o valor a ser aplicado só é aceito quando maior que o saldo disponivel na conta
(defun abrir-poupanca ()
    (format t "~%Digite o CPF a ser encontrado: ")
    ;;verifica se o CPF é valido
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                ;;caso seja valido os dados são coletados para abrir uma poupança
                (progn
                    ;;declara as variaveis
                    (let ( (tipo nil)
                            (valor-inicial nil)
                            (rendimento nil)
                            (data-inicial-rendimento nil)
                            (prazo-final nil)
                            (valor-atual nil)
                    ))

                    ;;coleta as informações para cada variavel
                    (setf tipo "Poupança")
                    (format t "~%Com qual valor você deseja iniciar: ")
                    (setf valor-inicial (parse-integer (read-line)))
                    ;;enquanto o valor for maior q o saldo da conta, o valor se da como invalido
                    (loop while (> valor-inicial (conta-saldo (cliente-conta busca-cliente)))
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja iniciar: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor-inicial (parse-integer (read-line)))
                        )
                    )
                    ;;salvando no historico de transação
                    (adicionar-ocorrencia-historico busca-cliente "Popança Aberta")
                    ;;desconta o valor que foi iniciado do saldo da conta
                    (retirar-valor-saldo valor-inicial busca-cliente)

                    ;;verifica a data atual do computador
                    (setf data-hoje (get-universal-time))
                    (setf data-inicial-rendimento (data-timestamp data-hoje))
                    
                    (setf rendimento 0.01)
                    (format t "~%Data final (XX/XX/XXXX): ")
                    (setf prazo-final (transformar-data (read-line)))
                    ;;calcula o rendimento
                    (setf valor-atual (calcular-rendimento-aplicacoes rendimento valor-inicial data-inicial-rendimento prazo-final))

                    ;;Registro no histórico de aplicações do cliente
                    (let ((motivo (format nil "Poupança aberta com valor de investimento de: ~,2f. Rendendo: ~,2f" valor-inicial valor-atual)))
                        (format t "~%Motivo: ~a" motivo)
                        (adicionar-ocorrencia-historico-aplicacoes busca-cliente motivo)
                    )

                    ;;salva na estrtura de aplicações
                    (let ((nova-aplicacao (make-aplicacoes :tipo tipo
                                            :valor-inicial valor-inicial
                                            :rendimento rendimento
                                            :data-inicial-investimento data-inicial-rendimento
                                            :prazo-final prazo-final
                                            :valor-atual valor-atual)))
                        (push nova-aplicacao (conta-aplicacoes (cliente-conta busca-cliente)))
                )
            )
            (format t "~%CPF não cadastrado.~%")
        )
    )
    )
)

;;abre um titulo de governo para o cliente identificado pelo CPF com seu rendimento de 0,02. O valor a ser aplicado só é aceito quando maior que o saldo disponivel na conta
(defun abrir-titulo-governo ()
    (format t "~%Digite o CPF a ser encontrado: ")
    ;;verifica se o CPF e valido
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
            ;;caso seja valido os dados são coletados para abrir um titulo do governo
                (progn
                    (let ( (tipo nil)
                            (valor-inicial nil)
                            (rendimento nil)
                            (data-inicial-rendimento nil)
                            (prazo-final nil)
                            (valor-atual nil)
                    ))

                    (setf tipo "Título do Governo")
                    (format t "~%Com qual valor você deseja iniciar: ")
                    (setf valor-inicial (parse-integer (read-line)))
                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                    (loop while (> valor-inicial (conta-saldo (cliente-conta busca-cliente)))
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja iniciar: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor-inicial (parse-integer (read-line)))
                        )
                    )
                    ;;salvando no historico de transação
                    (adicionar-ocorrencia-historico busca-cliente "Titulo do Governo Aberto.")
                    ;;desconta o valor que foi iniciado do saldo da conta
                    (retirar-valor-saldo valor-inicial busca-cliente)

                    ;;verifica a data atual do computador
                    (setf data-hoje (get-universal-time))
                    (setf data-inicial-rendimento (data-timestamp data-hoje))
                    
                    (setf rendimento 0.02)
                    (format t "~%Data final (XX/XX/XXXX): ")
                    (setf prazo-final (transformar-data (read-line)))
                    ;;calcula o rendimento
                    (setf valor-atual (calcular-rendimento-aplicacoes rendimento valor-inicial data-inicial-rendimento prazo-final))

                    ;;Registro no histórico de titulo do governo do cliente
                    (let ((motivo (format nil "Título do Governo aberto com valor de investimento de: ~,2f. Rendendo: ~,2f" valor-inicial valor-atual)))
                        (format t "~%Motivo: ~a" motivo)
                        (adicionar-ocorrencia-historico-aplicacoes busca-cliente motivo)
                    )

                    ;;salva na estrtura de aplicações
                    (let ((nova-aplicacao (make-aplicacoes  :tipo tipo
                                            :valor-inicial valor-inicial
                                            :rendimento rendimento
                                            :data-inicial-investimento data-inicial-rendimento
                                            :prazo-final prazo-final
                                            :valor-atual valor-atual)))
                        (push nova-aplicacao (conta-aplicacoes (cliente-conta busca-cliente)))
                )
            )
            (format t "~%CPF não cadastrado.~%")
        )
    )
    )
)

;;abre um fundo de investimento para o cliente identificado pelo CPF com seu rendimento de 0,03. O valor a ser aplicado só é aceito quando maior que o saldo disponivel na conta
(defun abrir-fundo-investimento ()
    (format t "~%Digite o CPF a ser encontrado: ")
    ;;verifica se o CPF é valido
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
            ;;caso seja valido os dados são coletados para abrir um fundo de investimento
                (progn
                    (let ( (tipo nil)
                            (valor-inicial nil)
                            (rendimento nil)
                            (data-inicial-rendimento nil)
                            (prazo-final nil)
                            (valor-atual nil)
                    ))

                    (setf tipo "Fundo de Investimento")
                    (format t "~%Com qual valor você deseja iniciar: ")
                    (setf valor-inicial (parse-integer (read-line)))
                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                    (loop while (> valor-inicial (conta-saldo (cliente-conta busca-cliente)))
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja iniciar: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor-inicial (parse-integer (read-line)))
                        )
                    )
                    
                    ;;salvando no historico de transação
                    (adicionar-ocorrencia-historico busca-cliente "Fundo de Investimento Aberto.")
                    ;;desconta o valor que foi iniciado do saldo da conta
                    (retirar-valor-saldo valor-inicial busca-cliente)

                    ;;verifica a data atual do computador
                    (setf data-hoje (get-universal-time))
                    (setf data-inicial-rendimento (data-timestamp data-hoje))
                    
                    (setf rendimento 0.03)
                    (format t "~%Data final (XX/XX/XXXX): ")
                    (setf prazo-final (transformar-data (read-line)))
                    ;;calcula o rendimento
                    (setf valor-atual (calcular-rendimento-aplicacoes rendimento valor-inicial data-inicial-rendimento prazo-final))

                    ;;Registro no histórico de fundo de investimento do cliente
                    (let ((motivo (format nil "Fundo de Investimento aberto com valor de investimento de: ~,2f. Rendendo: ~,2f" valor-inicial valor-atual)))
                        (format t "~%Motivo: ~a" motivo)
                        (adicionar-ocorrencia-historico-aplicacoes busca-cliente motivo)
                    )

                    ;;salva na estrtura de aplicações
                    (let ((nova-aplicacao (make-aplicacoes  :tipo tipo
                                            :valor-inicial valor-inicial
                                            :rendimento rendimento
                                            :data-inicial-investimento data-inicial-rendimento
                                            :prazo-final prazo-final
                                            :valor-atual valor-atual)))
                        (push nova-aplicacao (conta-aplicacoes (cliente-conta busca-cliente)))
                )
            )
            (format t "~%CPF não cadastrado.~%")
        )
    )
    )
)

(defun consultar-rendimentos ()
    (format t "~%Digite o CPF a ser encontrado: ")
    ;;verifica se o cpf é valido
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                ;;se for valido escreve o rendimento das aplicações
                (escrita-rendimento busca-cliente)
                ;;se nao
                (format t "~%CPF não cadastrado.~%")
            )
            (adicionar-ocorrencia-historico-aplicacoes busca-cliente "Rendimentos consultados")
        )
    )
)

(defun resgatar-rendimentos ()
    ;;resgata todos os rendiemntos e exclui-os da consulta
    (format t "~%Digite o CPF a ser encontrado: ")
    ;;verifica se o cpf é valido
    (let ((valor nil) (cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    (let ((aplicacoes (conta-aplicacoes (cliente-conta busca-cliente)))
                            (rendimento-total 0))
                        (dolist (aplicacao aplicacoes)
                            (let ((valor-atual (aplicacoes-valor-atual aplicacao)))
                                ;;adiciona o valor atual da aplicação ao saldo da conta
                                (setf rendimento-total (+ rendimento-total valor-atual))
                            )
                        )
                        
                        ;;salvando no historico de transação
                        (adicionar-ocorrencia-historico busca-cliente "Rendimentos resgatados")
                        ;; deposita o valor gerado no saldo
                        (depositar-valor-saldo rendimento-total busca-cliente)

                        ;;limpa a lista de aplicações
                        (setf (conta-aplicacoes (cliente-conta busca-cliente)) '())

                        ;; Retorna o rendimento total resgatado
                        (format t "~%Rendimento total resgatado: ~,2f~%" rendimento-total)

                        ;;Registro no histórico de fundo de investimento do cliente
                        (let ((motivo (format nil "Rendimentos de aplicações resgatados rendendo: ~,2f" valor-atual)))
                            (format t "~%Motivo: ~a" motivo)
                            (adicionar-ocorrencia-historico-aplicacoes busca-cliente motivo)
                        )
                    )
                )
                ;;se nao
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;adiciona 1 ocorrencia de movimentacao no historico de aplicações do cliente,, dizendo o motivo
(defun adicionar-ocorrencia-historico-aplicacoes (cliente motivo)
    (let* ((historico-aplicacao (conta-historico-aplicacoes (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-aplicacao (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-aplicacao historico-aplicacao)
        (setf (conta-historico-aplicacoes (cliente-conta cliente)) historico-aplicacao)
    )
)

;;adiciona 1 ocorrencia de movimentacao no historico de transferencias do cliente,, dizendo o motivo
(defun adicionar-ocorrencia-historico-transferencia (cliente motivo)
    (let* ((historico-transferencia (conta-historico-transferencias (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-transferencia (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-transferencia historico-transferencia)
        (setf (conta-historico-transferencias (cliente-conta cliente)) historico-transferencia)
    )
)

(defun tranferir-valor-entre-contas (origem destino valor)
    ;;desconta o valor para a conta de origem
    (retirar-valor-saldo valor origem)
    ;;depositar o valor para a conta de destino
    (depositar-valor-saldo valor destino)
)

;;realiza uma transferencias entre 2 contas existentes no banco de dados
(defun tranferencia-entre-contas ()
    (format t "~%Digite o CPF de origem a ser encontrado: ")
    ;;verifica se o cpf é valido
    (let ((valor nil) (cpf-busca (read-line)))
        (let ((busca-cliente-origem (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente-origem 
                (progn
                    (format t "~%Digite o CPF de destino a ser encontrado: ")
                    ;;verifica se o cpf é valido
                    (let ((cpf-busca-segundaria (read-line)))
                        (let ((busca-cliente-destino (verifica-existencia-cpf cpf-busca-segundaria)))
                            (if busca-cliente-destino
                                ;;se for valido verifica os valores do rendimento
                                (progn
                                    (format t "~%Qual valor você deseja tranferir: ")
                                    (setf valor (parse-integer (read-line)))
                                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                                    (loop while (> valor (conta-saldo (cliente-conta busca-cliente-origem)))
                                        do (progn 
                                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                                            (format t "~%Com qual valor você deseja transferir: ")
                                            ;;parse-integer transforma a string em numero
                                            (setf valor (parse-integer (read-line)))
                                        )
                                    )
                                    ;;salvando no historico da conta
                                    (adicionar-ocorrencia-historico busca-cliente-origem "Transferência realizada")

                                    (tranferir-valor-entre-contas busca-cliente-origem busca-cliente-destino valor)
                                    (format t "~%Tranferência realizada com sucesso~%")

                                    ;;Registro no histórico do cliente de origem
                                    (let ((motivo (format nil "Transferência no valor de ~a para CPF ~a" valor (cliente-cpf busca-cliente-destino))))
                                        (format t "~%Motivo: ~a" motivo)
                                        (adicionar-ocorrencia-historico-transferencia busca-cliente-origem motivo)
                                    )

                                    ;;Registro no histórico do cliente de destino
                                    (let ((motivo (format nil "Recebido de CPF: ~a o valor de ~a" (cliente-cpf busca-cliente-origem) valor)))
                                        (adicionar-ocorrencia-historico-transferencia busca-cliente-destino motivo)
                                    )
                                )
                                ;;se nao
                                (format t "~%CPF não cadastrado.~%")
                            )
                            )
                        )
                    )
                    ;;se nao
                    (format t "~%CPF não cadastrado.~%")
                )
            )
        )
)

;;escreve o histórico de transferencias 
(defun escrita-historico-transferencia (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a~%"
                (cliente-nome cliente)
    )
    (dolist (ocorrencia (conta-historico-transferencias (cliente-conta cliente)))
        ;;separa a lista em 2, pois a primeira é a data e a segunda ao motivo
        (let* ((data (first ocorrencia)) 
                (motivo (second ocorrencia))
            )  
            (format t "     Data: ~a/~a/~a - Motivo: ~a~%"
                (first data) (second data) (third data) motivo
            )
        )
    )
)

;;verifica se o cpf é valido e chama a função para escrever o histórico de transferências
(defun historico-transferencia ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (escrita-historico-transferencia busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;adiciona 1 ocorrencia de movimentacao no historico de pagamentos do cliente, dizendo o motivo
(defun adicionar-ocorrencia-historico-pagamentos (cliente motivo)
    (let* ((historico-pagamento (conta-historico-pagamentos (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-pagamento (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-pagamento historico-pagamento)
        (setf (conta-historico-pagamentos (cliente-conta cliente)) historico-pagamento)
    )
)

;;realiza o pagamento de conta do cliente
(defun pagar-conta()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((valor nil) (cpf-busca (read-line)) (destinatario nil))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    (format t "~%Para quem você deseja realizar o pagamento: ")
                    (setf destinatario (read-line))
                    (format t "~%Qual valor você deseja pagar: ")
                    (setf valor (parse-integer (read-line)))
                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                    (loop while (> valor (conta-saldo (cliente-conta busca-cliente)))
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja transferir: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor (parse-integer (read-line)))
                        )
                    )
                    ;;salvando no historico da conta
                    (adicionar-ocorrencia-historico busca-cliente "Pagamento realizado")

                    ;;desconta o valor para a conta de origem
                    (retirar-valor-saldo valor busca-cliente)
                    (format t "~%Pagamento realizada com sucesso para ~a~%" destinatario)

                    ;;Registro no histórico de pagamento cliente de destino
                    (let ((motivo (format nil "Pagamento no valor de ~a para: ~a" valor destinatario)))
                        (adicionar-ocorrencia-historico-pagamentos busca-cliente motivo)
                    )
                )
                ;;se nao
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;escreve o histórico de pagamentos
(defun escrita-historico-pagamentos (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a
                Agencia: ~a~%"
                (cliente-nome cliente)
                (conta-agencia (cliente-conta cliente))
    )
    (dolist (ocorrencia (conta-historico-pagamentos (cliente-conta cliente)))
        ;;separa a lista em 2, pois a primeira é a data e a segunda ao motivo
        (let* ((data (first ocorrencia)) 
                (motivo (second ocorrencia))
            )  
            (format t "     Data: ~a/~a/~a - Motivo: ~a~%"
                (first data) (second data) (third data) motivo
            )
        )
    )
)

;;verifica se o cpf é valido e chama a função para escrever o histórico de pagamentos
(defun historico-pagamento ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (escrita-historico-pagamentos busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

(defun limite-de-saque (x)
    (* x 0.90)
)

;;adiciona 1 ocorrencia de movimentacao no historico de saques do cliente, dizendo o motivo
(defun adicionar-ocorrencia-historico-saque (cliente motivo)
    (let* ((historico-saque (conta-historico-saque (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-saque (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-saque historico-saque)
        (setf (conta-historico-saque (cliente-conta cliente)) historico-saque)
    )
)

(defun realizar-saque ()
(format t "~%Digite o CPF a ser encontrado: ")
    (let ((valor nil) (cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    (format t "~%Qual valor você deseja sacar: ")
                    (setf valor (parse-integer (read-line)))
                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                    (loop while (> valor (limite-de-saque (conta-saldo (cliente-conta busca-cliente))))
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja transferir: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor (parse-integer (read-line)))
                        )
                    )

                    ;;salvando no historico da conta
                    (adicionar-ocorrencia-historico busca-cliente "Saque realizado")

                    ;;desconta o valor para a conta de origem
                    (retirar-valor-saldo valor busca-cliente)
                    (format t "~%Saque realizada com sucesso para ~a~%" (cliente-nome busca-cliente))

                    ;;Registro no histórico de pagamento cliente de destino
                    (let ((motivo (format nil "Saque no valor de ~a." valor)))
                        (adicionar-ocorrencia-historico-saque busca-cliente motivo)
                    )
                )
                ;;se nao
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;escreve o histórico de saques
(defun escrita-historico-saque (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a~%"
                (cliente-nome cliente)
    )
    (dolist (ocorrencia (conta-historico-saque (cliente-conta cliente)))
        ;;separa a lista em 2, pois a primeira é a data e a segunda ao motivo
        (let* ((data (first ocorrencia)) 
                (motivo (second ocorrencia))
            )  
            (format t "     Data: ~a/~a/~a - Motivo: ~a~%"
                (first data) (second data) (third data) motivo
            )
        )
    )
)

;;verifica se o cpf é valido e chama a função para escrever o histórico de saques
(defun historico-saque ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (escrita-historico-saque busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;adiciona 1 ocorrencia de movimentacao no historico de depositos do cliente, dizendo o motivo
(defun adicionar-ocorrencia-historico-deposito (cliente motivo)
    (let* ((historico-deposito (conta-historico-deposito (cliente-conta cliente)))
            ;;capta a data atual do computador
            (data-atual (data-timestamp (get-universal-time)))
            (adicionar-deposito (list data-atual motivo))
        )
        ;;adiciona no historico da conta
        (push adicionar-deposito historico-deposito)
        (setf (conta-historico-deposito (cliente-conta cliente)) historico-deposito)
    )
)

(defun realizar-deposito ()
(format t "~%Digite o CPF a ser encontrado: ")
    (let ((valor nil) (cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    (format t "~%Qual valor você deseja depositar: ")
                    (setf valor (parse-integer (read-line)))
                    ;;caso o valor seja menor que o do saldo ele pergunta um novo valor
                    (loop while (< valor 0.01)
                        do (progn 
                            (format t "~%Valor inválido. O valor não pode ser maior que o saldo da conta.")
                            (format t "~%Com qual valor você deseja transferir: ")
                            ;;parse-integer transforma a string em numero
                            (setf valor (parse-integer (read-line)))
                        )
                    )

                    ;;salvando no historico da conta
                    (adicionar-ocorrencia-historico busca-cliente "Depósito realizado")

                    ;;adicionar o valor para a conta de origem
                    (depositar-valor-saldo valor busca-cliente)
                    (format t "~%Depósito realizada com sucesso para ~a~%" (cliente-nome busca-cliente))

                    ;;Registro no histórico de pagamento cliente de destino
                    (let ((motivo (format nil "Depósito no valor de ~a." valor)))
                        (adicionar-ocorrencia-historico-deposito busca-cliente motivo)
                    )
                )
                ;;se nao
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;escreve o histórico de depositos
(defun escrita-historico-deposito (cliente)
    (format t "~%Histórico da conta:
                Cliente: ~a~%"
                (cliente-nome cliente)
    )
    (dolist (ocorrencia (conta-historico-deposito (cliente-conta cliente)))
        ;;separa a lista em 2, pois a primeira é a data e a segunda ao motivo
        (let* ((data (first ocorrencia)) 
                (motivo (second ocorrencia))
            )  
            (format t "     Data: ~a/~a/~a - Motivo: ~a~%"
                (first data) (second data) (third data) motivo
            )
        )
    )
)

;;verifica se o cpf é valido e chama a função para escrever o histórico de depositos
(defun historico-deposito ()
    (format t "~%Digite o CPF a ser encontrado: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (escrita-historico-deposito busca-cliente)
                (format t "~%CPF não cadastrado.~%")
            )
        )
    )
)

;;função que gera o relatorio de transferencia para arquivo CSV
(defun gerar-relatorio-transferencia (cliente)
    (let ((historico-transferencias (conta-historico-transferencias (cliente-conta cliente)))
        (arquivo (format nil "relatorio-transferencias-~a.csv" (cliente-cpf cliente))))
        (with-open-file (stream arquivo
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
            (format stream "RELATÓRIO DE TRANSFERÊNCIAS~%~%")
            (format stream "Data,Motivo~%")
            ;;le a lista de trsnferencias até que ache o final dela, separando pelo motivo e a data
            (dolist (transferencia historico-transferencias)
                (let ((data (first transferencia))
                    (motivo (second transferencia)))
                    ;;formato que irá aparecer no arquivo
                    (format stream "~a,~a~%" data motivo)
                )
            )
        )
    )
    (format t "Relatório de transferências gerado com sucesso!~%")
    (adicionar-ocorrencia-historico cliente "Relatório de Transferências gerado com sucesso!")
    (logar)
)

;;função que gera o relatorio de pagamento para arquivo CSV
(defun gerar-relatorio-pagamento (cliente)
    (let ((historico-pagamentos (conta-historico-pagamentos (cliente-conta cliente)))
        (arquivo (format nil "relatorio-pagamentos-~a.csv" (cliente-cpf cliente))))
        (with-open-file (stream arquivo
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
            (format stream "RELATÓRIO DE PAGAMENTOS~%~%")
            (format stream "Data,Motivo~%")
            ;;le a lista de trsnferencias até que ache o final dela, separando pelo motivo e a data
            (dolist (pagamento historico-pagamentos)
                (let ((data (first pagamento))
                    (motivo (second pagamento)))
                    ;;formato que irá aparecer no arquivo
                    (format stream "~a,~a~%" data motivo)
                )
            )
        )
    )
    (format t "Relatório de pagamentos gerado com sucesso!~%")
    (adicionar-ocorrencia-historico cliente "Relatório de Pagamentos gerado com sucesso!")
    (logar)
)

;;função que gera o relatorio de aplicações para arquivo CSV
(defun gerar-relatorio-aplicacoes (cliente)
    (let ((historico-aplicacoes (conta-historico-aplicacoes (cliente-conta cliente)))
        (arquivo (format nil "relatorio-aplicacoes-~a.csv" (cliente-cpf cliente))))
        (with-open-file (stream arquivo
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
            (format stream "RELATÓRIO DE APLICAÇÕES~%~%")
            (format stream "Data,Motivo~%")
            ;;le a lista de trsnferencias até que ache o final dela, separando pelo motivo e a data
            (dolist (aplicacao historico-aplicacoes)
                (let ((data (first aplicacao))
                    (motivo (second aplicacao)))
                    ;;formato que irá aparecer no arquivo
                    (format stream "~a,~a~%" data motivo)
                )
            )
        )
    )
    (format t "Relatório de aplicacoes gerado com sucesso!~%")
    (adicionar-ocorrencia-historico cliente "Relatório de Aplicacoes gerado com sucesso!")
    (logar)
)

(defun gerar-relatorio-desempenho ()
    (let ((arquivo "relatorio-desempenho-agencia.csv")
        (lista-de-clientes *clientes*))
        ;;calcula o total de novos clientes
        (let* ((total-clientes (length lista-de-clientes)))
        ;;calcula o total de transações
            (let ((total-transferencias (count-if #'(lambda (cliente)
                                                    (> (length (conta-historico-transferencias (cliente-conta cliente))) 0))
                                                lista-de-clientes)))
                ;;calcula o total de depósitos
                (let ((total-deposito (count-if #'(lambda (cliente)
                                                    (> (length (conta-historico-deposito (cliente-conta cliente))) 0))
                                                lista-de-clientes)))
                    ;;calcula o total de aplicações
                    (let ((total-aplicacoes (count-if #'(lambda (cliente)
                                                            (> (length (conta-aplicacoes (cliente-conta cliente))) 0))
                                                        lista-de-clientes)))
                        ;;gera o relatorio separado pela metreica e o total
                        (with-open-file (stream arquivo
                                                :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                        (format stream "RELATÓRIO DE DESEMPENHO DA AGÊNCIA~%~%")
                        (format stream "Métrica,Total~%")
                        (format stream "Total de Transferências,~a~%" total-transferencias)
                        (format stream "Total de Novos Clientes,~a~%" total-clientes)
                        (format stream "Total de Depósitos,~a~%" total-deposito)
                        (format stream "Total de Aplicações,~a~%" total-aplicacoes))
                    )
                )
            )
        )
    )
    (format t "~%Relatório de desempenho da agência gerado com sucesso!~%")
    (gerenciamento-relatorios-gerais)
)

;;função para buscar o cliente e gerar o relatório CSV
(defun gerar-relatorios (escolha)
    (format t "~%Digite o CPF do cliente para gerar o relatório de transferências: ")
    (let ((cpf-busca (read-line)))
        (let ((busca-cliente (verifica-existencia-cpf cpf-busca)))
            (if busca-cliente
                (progn
                    (loop
                        (case escolha
                            (1 (gerar-relatorio-transferencia busca-cliente))
                            (2 (gerar-relatorio-pagamento busca-cliente))
                            (3 (gerar-relatorio-aplicacoes busca-cliente))
                            (otherwise (format t "Escolha não existe. Tente novamente!"))
                        )
                    )
                )
                )
                (format t "~%CPF não cadastrado.~%")
            )
        )
)


;;inicio do gerenciamento de menus
(defun gerenciamento-relatorios-gerais ()
    ;;gereciamento do menu de relatorios gerais
    (loop
        (menu-gerenciamento-relatorios-gerais)
        (let ((escolha (read)))
        (case escolha
            ;;chama a função gerar-relatorios com a numeração para verificar o cpf do cliente que o usuario deseja gerar o relatorio CSV
            (1 (gerar-relatorios 1))
            (2 (gerar-relatorios 2))
            (3 (gerar-relatorios 3))
            (4 (gerar-relatorio-desempenho))
            (5 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun gerenciamento-saque-deposito ()
    ;;gereciamento do menu de saques e depositos
    (loop
        (menu-gerenciamento-saque-deposito)
        (let ((escolha (read)))
        (case escolha
            (1 (realizar-saque))
            (2 (historico-saque))
            (3 (realizar-deposito))
            (4 (historico-deposito))
            (5 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun gerenciamento-pagamentos ()
    ;;gereciamento do menu de pagamentos
    (loop
        (menu-gerenciamento-pagamentos)
        (let ((escolha (read)))
        (case escolha
            (1 (pagar-conta))
            (2 (historico-pagamento))
            (3 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun gerenciamento-transferencias ()
    ;;gereciamento do menu de transferencias
    (loop
        (menu-gerenciamento-transferencias)
        (let ((escolha (read)))
        (case escolha
            (1 (tranferencia-entre-contas))
            (2 (historico-transferencia))
            (3 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun gerenciamento-aplicacoes ()
    ;;gereciamento do menu de aplicações
    (loop
        (menu-gerenciamento-aplicacoes)
        (let ((escolha (read)))
        (case escolha
            (1 (abrir-poupanca))
            (2 (abrir-titulo-governo))
            (3 (abrir-fundo-investimento))
            (4 (consultar-rendimentos))
            (5 (resgatar-rendimentos))
            (6 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun gerenciamento-clientes-contas ()
    ;;gereciamento do menu de contas dos clientes
    (loop
        (menu-gerenciamento-cliente)
        (let ((escolha (read)))
        (case escolha
            (1 (cadastrar-novo-cliente))
            (2 (atualizar-dados-cliente))
            (3 (buscar-cadastro-cliente))
            (4 (consultar-saldo-conta))
            (5 (consultar-historico-conta))
            (6 (encerrar-conta))
            (7 (logar))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)

(defun logar()
    ;;gereciamento do menu principal
    (loop
        (menu-principal)
        (let ((escolha (read)))
        (case escolha
            (1 (gerenciamento-clientes-contas))
            (2 (gerenciamento-aplicacoes))
            (3 (gerenciamento-transferencias))
            (4 (gerenciamento-pagamentos))
            (5 (gerenciamento-saque-deposito))
            (6 (gerenciamento-relatorios-gerais))
            (7 (format t "Obrigado por visitar nosso sistema!") (exit))
            (otherwise (format t "Escolha não existe. Tente novamente!"))
        )
        )
    )
)