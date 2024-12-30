# **Documentação de Testes E2E - Adicionar Condição Médica (Frontend)**

**Módulo:** Interface de Adição de Condição Médica  
**Objetivo:** Validar o funcionamento da interface de adição de condições médicas, incluindo a exibição correta do formulário, envio bem-sucedido, manipulação de erros e validação de campos obrigatórios.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de adição de condição médica** | Acessar a página de adição de condição médica e verificar os elementos do formulário. | **Status:** 200. Os campos `name`, `severity`, `description` e o botão `Add Medical Condition` estão visíveis. |
| **Adicionar uma nova condição médica com sucesso** | Preencher corretamente o formulário e enviar os dados. | **Status:** 200 ou 201. **Mensagem:** "Medical condition added successfully!". O formulário é limpo após envio bem-sucedido. |
| **Lidar com erros de rede** | Simular uma falha no servidor ao adicionar uma nova condição médica. | **Status:** 500. **Mensagem:** "Internal Server Error". Uma mensagem de erro visível é exibida. |
| **Validar campos obrigatórios** | Tentar enviar o formulário com campos obrigatórios vazios. | O navegador exibe mensagens de validação nos campos obrigatórios (`name`, `description`). |
| **Persistir dados do formulário após falha na submissão** | Submeter dados inválidos e verificar se os dados permanecem nos campos do formulário. | Os campos `name`, `severity` e `description` mantêm os valores preenchidos após falha. |
| **Selecionar diferentes níveis de severidade** | Testar a seleção das opções `Low`, `Medium` e `High` no campo `severity`. | O campo `severity` permite selecionar e refletir corretamente os valores `Low`, `Medium` e `High`. |

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- Interceptações (`cy.intercept`) foram usadas para simular respostas do backend, incluindo falhas controladas.
- Cenários foram validados tanto para sucesso quanto para falhas de rede e validação.
- A persistência de dados após falhas de validação foi verificada com sucesso.

---

**Última atualização:** `{{date}}`
