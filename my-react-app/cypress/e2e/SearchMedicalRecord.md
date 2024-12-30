# **Documentação de Testes E2E - Buscar Registros Médicos (Frontend)**

**Módulo:** Interface de Busca de Registros Médicos  
**Objetivo:** Validar o funcionamento da interface de busca de registros médicos, incluindo filtros por ID de paciente, condições, alergias e manipulação de erros.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Buscar registro médico apenas pelo ID do paciente** | Preencher o campo `Patient ID` e clicar em `Search`. | **Status:** 200. Os detalhes do registro médico são exibidos com as condições e alergias associadas. |
| **Buscar registro médico pelo ID do paciente e condição específica** | Preencher `Patient ID` e `Condition Name`, clicar em `Search`. | **Status:** 200. Apenas as condições correspondentes ao filtro são exibidas. |
| **Buscar registro médico pelo ID do paciente e alergia específica** | Preencher `Patient ID` e `Allergy Name`, clicar em `Search`. | **Status:** 200. Apenas as alergias correspondentes ao filtro são exibidas. |
| **Buscar com todos os filtros aplicados (ID, condição e alergia)** | Preencher `Patient ID`, `Condition Name` e `Allergy Name`, clicar em `Search`. | **Status:** 200. Somente as condições e alergias que atendem a todos os filtros são exibidas. |
| **Buscar com ID de paciente inexistente** | Preencher `Patient ID` com um valor inexistente e clicar em `Search`. | **Status:** 404. **Mensagem:** "Medical record not found". Uma mensagem de erro é exibida. |
| **Lidar com erros do servidor** | Simular um erro no servidor durante a busca. | **Status:** 500. **Mensagem:** "Server error". Uma mensagem de erro é exibida. |

---

## **Detalhes dos Testes**

### **1. Buscar registro médico apenas pelo ID do paciente**  
- **Ação:** Preencher o campo `Patient ID` com um valor válido e clicar em `Search`.  
- **Validação:**  
   - Os detalhes do registro médico são exibidos.  
   - Condições como `Asthma`, `Diabetes` aparecem corretamente.  
   - Alergias como `Peanuts`, `Shellfish` aparecem corretamente.  

### **2. Buscar registro médico pelo ID do paciente e condição específica**  
- **Ação:** Preencher `Patient ID` com um valor válido e `Condition Name` com `Asthma`.  
- **Validação:**  
   - Somente a condição `Asthma` é exibida.  
   - A condição `Diabetes` não aparece.  

### **3. Buscar registro médico pelo ID do paciente e alergia específica**  
- **Ação:** Preencher `Patient ID` com um valor válido e `Allergy Name` com `Peanuts`.  
- **Validação:**  
   - Somente a alergia `Peanuts` é exibida.  
   - A alergia `Shellfish` não aparece.  

### **4. Buscar com todos os filtros aplicados (ID, condição e alergia)**  
- **Ação:** Preencher `Patient ID`, `Condition Name` com `Asthma`, e `Allergy Name` com `Peanuts`.  
- **Validação:**  
   - Apenas a condição `Asthma` e a alergia `Peanuts` são exibidas.  
   - `Diabetes` e `Shellfish` não aparecem nos resultados.  

### **5. Buscar com ID de paciente inexistente**  
- **Ação:** Preencher o campo `Patient ID` com um valor inexistente e clicar em `Search`.  
- **Validação:**  
   - Uma mensagem de erro é exibida: **"Medical record not found"**.  
   - Nenhum resultado é exibido na interface.  

### **6. Lidar com erros do servidor**  
- **Ação:** Simular um erro do servidor ao buscar registros médicos.  
- **Validação:**  
   - Uma mensagem de erro é exibida: **"Server error"**.  
   - Nenhum resultado é exibido na interface.  

---

## **Fluxo do Teste**

1. **Autenticação:** Login válido com um token JWT.  
2. **Criação de dados de teste:** Registro médico com condições e alergias específicas é criado.  
3. **Execução dos testes:** Testes validam cada cenário de busca.  
4. **Limpeza:** O registro médico de teste é excluído após a execução dos testes.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões médicas.
- Um registro médico temporário foi criado antes dos testes e removido após sua conclusão.
- Respostas controladas do backend foram simuladas para testar falhas de servidor.
- Cada cenário validou corretamente tanto os casos de sucesso quanto os de falha.

---

**Última atualização:** `{{date}}`
