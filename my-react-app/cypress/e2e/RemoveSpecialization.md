# **Documentação de Testes E2E - Remover Especialização (Frontend)**

**Módulo:** Interface de Remoção de Especialização  
**Objetivo:** Validar o processo de remoção de uma especialização no sistema, incluindo a exibição correta do modal de confirmação, a remoção bem-sucedida e o cancelamento da operação.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir modal de confirmação de exclusão** | Clicar no botão de exclusão de uma especialização e validar o modal de confirmação. | **Status:** 200. O modal de exclusão é exibido com as opções `Yes, Delete` e `Cancel`. |
| **Excluir uma especialização com sucesso** | Confirmar a exclusão no modal de confirmação. | **Status:** 200. **Mensagem:** "Specialization successfully deleted". O usuário é redirecionado para a página `/specializations/search`. |
| **Cancelar o processo de exclusão** | Clicar no botão `Cancel` no modal de confirmação. | **Status:** 200. O modal é fechado, e o usuário permanece na página `/specializations/search`. |

---

## **Detalhes dos Testes**

### **1. Exibir modal de confirmação de exclusão**  
- **Ação:** Selecionar uma especialização, clicar no botão de exclusão e validar o modal exibido.  
- **Validação:** O modal contém:  
   - **Título:** "Delete Specialization"  
   - **Mensagem:** "Are you sure you want to delete this specialization?"  
   - Botões visíveis: `Yes, Delete` e `Cancel`.  

### **2. Excluir uma especialização com sucesso**  
- **Ação:** Confirmar a exclusão no modal clicando no botão `Yes, Delete`.  
- **Validação:** Uma mensagem de sucesso é exibida: **"Specialization successfully deleted"**.  
- **Redirecionamento:** O usuário retorna para a página `/specializations/search`.  

### **3. Cancelar o processo de exclusão**  
- **Ação:** No modal de confirmação, clicar no botão `Cancel`.  
- **Validação:** O modal é fechado, e o usuário permanece na página `/specializations/search`.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- A especialização usada para os testes foi criada dinamicamente durante a fase `before`.
- O modal de confirmação foi validado para conter as opções esperadas.
- A remoção foi verificada tanto para sucesso quanto para cancelamento.

---

**Última atualização:** `{{date}}`
