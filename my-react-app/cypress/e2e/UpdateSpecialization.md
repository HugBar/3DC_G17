# **Documentação de Testes E2E - Atualizar Especialização (Frontend)**

**Módulo:** Interface de Atualização de Especializações  
**Objetivo:** Validar o funcionamento da interface de atualização de especializações, incluindo exibição de dados existentes, validação de campos obrigatórios, manipulação de erros e atualização bem-sucedida.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de atualização de especialização** | Acessar a página de atualização de uma especialização existente e validar os elementos do formulário. | **Status:** 200. Os campos são preenchidos com os dados existentes da especialização. |
| **Atualizar uma especialização com sucesso** | Preencher os campos com novos valores e clicar em `Update Specialization`. | **Status:** 200. **Mensagem:** "Specialization updated successfully!". |
| **Validar campos obrigatórios** | Deixar os campos `Name` e `Description` vazios e tentar atualizar. | **Status:** 400. O navegador exibe mensagens de validação nos campos obrigatórios. |
| **Manipular erro para especialização inexistente** | Tentar acessar a página de atualização de uma especialização inexistente. | **Status:** 404. **Mensagem:** "Specialization not found". |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de atualização de especialização**  
- **Ação:** Acessar a página `/specialization/update/{testSpecialization.name}`.  
- **Validação:**  
   - O título `Update Specialization` está visível.  
   - O campo `#name` contém o nome atual da especialização.  
   - O campo `#description` contém a descrição atual.  
   - O botão `Update Specialization` está visível.  

---

### **2. Atualizar uma especialização com sucesso**  
- **Ação:**  
   - Alterar o campo `#name` para `Updated Test Specialization`.  
   - Alterar o campo `#description` para `Updated Test Description`.  
   - Clicar no botão `Update Specialization`.  
- **Validação:**  
   - Uma mensagem de sucesso é exibida: **"Specialization updated successfully!"**.  
   - A API retorna status **200 OK**.  

---

### **3. Validar campos obrigatórios**  
- **Ação:**  
   - Deixar os campos `#name` e `#description` vazios.  
   - Clicar no botão `Update Specialization`.  
- **Validação:**  
   - O navegador exibe mensagens de validação nos campos obrigatórios.  
   - A especialização não é atualizada.  

---

### **4. Manipular erro para especialização inexistente**  
- **Ação:** Acessar a URL `/specialization/update/NonExistentSpecialization`.  
- **Validação:**  
   - Uma mensagem de erro é exibida: **"Specialization not found"**.  
   - A interface não exibe o formulário de atualização.  

---

## **Fluxo do Teste**

1. **Autenticação:** Login com token JWT de usuário administrador.  
2. **Criação de Dados de Teste:** Uma especialização temporária é criada antes dos testes.  
3. **Execução dos Testes:** Validação de exibição correta do formulário, sucesso na atualização, validação de campos obrigatórios e manipulação de erros.  
4. **Limpeza:** A especialização de teste é removida após a conclusão dos testes.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- Respostas controladas do backend foram interceptadas para validar cenários de erro.
- Cada teste validou corretamente os comportamentos esperados para sucesso e falha.

---

**Última atualização:** `{{date}}`
