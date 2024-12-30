# **Documentação de Testes E2E - Atualizar Registro Médico (Frontend)**

**Módulo:** Interface de Atualização de Registros Médicos  
**Objetivo:** Validar o funcionamento da interface de atualização de registros médicos, incluindo a adição e remoção de condições e alergias, manipulação de erros e confirmação de sucesso na atualização.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de atualização** | Acessar a página de atualização de um registro médico e validar os elementos do formulário. | **Status:** 200. Os campos e seções para condições e alergias estão visíveis. |
| **Adicionar e remover condições e alergias** | Adicionar uma condição e uma alergia ao registro e removê-las em seguida. | **Status:** 200. As condições e alergias são exibidas/removidas corretamente na interface. |
| **Atualizar registro médico com sucesso** | Preencher as informações necessárias e clicar em `Update`. | **Status:** 200. **Mensagem:** "Medical record updated successfully". |
| **Lidar com erros na atualização do registro** | Simular uma falha no servidor durante a atualização. | **Status:** 500. **Mensagem:** "Failed to update medical record". Uma mensagem de erro é exibida. |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de atualização**  
- **Ação:** Acessar a página `/medical-records/update/{testPatientId}`.  
- **Validação:**  
   - O título `Update Medical Record` está visível.  
   - As seções `Medical Conditions` e `Allergies` estão visíveis.  
   - O seletor `#conditionSelect` está presente.  
   - O seletor para alergias também está presente.  

---

### **2. Adicionar e remover condições e alergias**  
- **Ação:**  
   - Selecionar `Asthma - High` no seletor de condições.  
   - Verificar se `Asthma` aparece na lista.  
   - Remover a condição selecionada.  
   - Selecionar `peanut - High` no seletor de alergias.  
   - Verificar se `peanut` aparece na lista.  
- **Validação:**  
   - A condição `Asthma` é adicionada e removida corretamente.  
   - A alergia `peanut` é exibida corretamente na lista.  

---

### **3. Atualizar registro médico com sucesso**  
- **Ação:**  
   - Adicionar `Asthma - High` como condição.  
   - Adicionar `peanut - High` como alergia.  
   - Clicar no botão `Update`.  
- **Validação:**  
   - Uma mensagem de sucesso aparece: **"Medical record updated successfully"**.  
   - As informações persistem após o recarregamento da página.  

---

### **4. Lidar com erros na atualização do registro**  
- **Ação:**  
   - Simular uma falha no servidor ao interceptar o endpoint de atualização (`500 Internal Server Error`).  
   - Tentar atualizar o registro médico.  
- **Validação:**  
   - Uma mensagem de erro aparece: **"Failed to update medical record"**.  
   - O registro não é atualizado.  

---

## **Fluxo do Teste**

1. **Autenticação:** Login válido com token JWT de um usuário com permissões médicas.  
2. **Criação de dados de teste:** Um registro médico temporário é criado antes dos testes.  
3. **Execução dos testes:** Cenários validam elementos do formulário, adição/remoção de dados, atualizações bem-sucedidas e tratamento de erros.  
4. **Limpeza:** O registro médico de teste é excluído após a execução dos testes.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões médicas.
- Foi validada tanto a experiência do usuário ao adicionar/remover dados quanto a robustez contra erros do servidor.
- O fluxo de atualização inclui mensagens claras para sucesso e falha.

---

**Última atualização:** `{{date}}`
