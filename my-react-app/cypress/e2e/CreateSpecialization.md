# **Documentação de Testes E2E - Criar Especialização (Frontend)**

**Módulo:** Interface de Criação de Especialização  
**Objetivo:** Validar o funcionamento da interface de criação de especializações, incluindo a exibição correta do formulário, envio bem-sucedido, manipulação de erros e validação de campos obrigatórios.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de criação de especialização** | Acessar a página de criação de especialização e verificar os elementos do formulário. | **Status:** 200. Os campos `name`, `description` e o botão `Add Specialization` estão visíveis. |
| **Adicionar uma nova especialização com sucesso** | Preencher corretamente o formulário e enviar os dados. | **Status:** 200 ou 201. **Mensagem:** "Specialization added successfully!". O formulário é limpo após envio bem-sucedido. |
| **Lidar com erros de rede** | Simular uma falha no servidor ao adicionar uma nova especialização. | **Status:** 500. **Mensagem:** "Internal Server Error". Uma mensagem de erro visível é exibida. |
| **Validar campos obrigatórios** | Tentar enviar o formulário com campos obrigatórios vazios. | O navegador exibe mensagens de validação nos campos obrigatórios (`name`, `description`). |
| **Persistir dados do formulário após falha na submissão** | Submeter dados inválidos e verificar se os dados permanecem nos campos do formulário. | Os campos `name` e `description` mantêm os valores preenchidos após falha. |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de criação de especialização**  
- **Ação:** Acessar a página `/specialization/create`.  
- **Validação:** Os campos `#name`, `#description` e o botão de envio estão visíveis.  

### **2. Adicionar uma nova especialização com sucesso**  
- **Ação:** Preencher os campos `name` e `description` e clicar no botão `Add Specialization`.  
- **Validação:** O envio retorna status **200/201**, e uma mensagem de sucesso é exibida: **"Specialization added successfully!"**.  

### **3. Lidar com erros de rede**  
- **Ação:** Simular uma falha no servidor durante o envio dos dados.  
- **Validação:** O envio retorna status **500**, e uma mensagem de erro é exibida: **"Internal Server Error"**.  

### **4. Validar campos obrigatórios**  
- **Ação:** Clicar no botão `Add Specialization` sem preencher os campos obrigatórios.  
- **Validação:** O navegador exibe mensagens de validação para os campos obrigatórios.  

### **5. Persistir dados do formulário após falha na submissão**  
- **Ação:** Preencher os campos `name` e `description`, simular erro no envio e verificar os valores após a falha.  
- **Validação:** Os valores permanecem nos campos após o erro.

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- Interceptações (`cy.intercept`) foram usadas para simular respostas do backend, incluindo falhas controladas.
- Cenários foram validados tanto para sucesso quanto para falhas de rede e validação.
- A persistência de dados após falhas de validação foi verificada com sucesso.

---

**Última atualização:** `{{date}}`
