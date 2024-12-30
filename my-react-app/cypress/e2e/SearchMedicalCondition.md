# **Documentação de Testes E2E - Buscar Condições Médicas (Frontend)**

**Módulo:** Interface de Busca de Condições Médicas  
**Objetivo:** Validar o funcionamento da interface de busca de condições médicas, incluindo filtros, manipulação de resultados, tratamento de erros e interação com os resultados da busca.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de busca de condições médicas** | Validar que os elementos do formulário estão visíveis (`Name`, `Severity`, botões `Search` e `Clear Filters`). | **Status:** 200. Os elementos do formulário estão visíveis e funcionais. |
| **Buscar condições médicas pelo nome** | Preencher o campo `Name` e clicar em `Search`. | **Status:** 200. Resultados compatíveis com o nome buscado são exibidos. |
| **Buscar condições médicas pela severidade** | Selecionar uma opção no campo `Severity` e clicar em `Search`. | **Status:** 200. Resultados compatíveis com a severidade buscada são exibidos. |
| **Buscar condições médicas com ambos os filtros (nome e severidade)** | Preencher `Name`, selecionar `Severity` e clicar em `Search`. | **Status:** 200. Resultados correspondem aos dois filtros aplicados. |
| **Limpar filtros de busca** | Preencher filtros e clicar em `Clear Filters`. | **Status:** 200. Os campos são redefinidos e os parâmetros são removidos da URL. |
| **Exibir detalhes da condição médica** | Clicar em um cartão de condição médica para visualizar detalhes. | **Status:** 200. Uma modal exibe detalhes sobre a condição selecionada. |
| **Lidar com busca sem resultados** | Buscar por uma condição médica inexistente. | **Status:** 404. Uma mensagem "Nenhuma condição médica encontrada." é exibida. |
| **Tratar erros do servidor** | Simular um erro no servidor durante a busca. | **Status:** 500. **Mensagem:** "Erro ao buscar condições médicas." Uma mensagem de erro é exibida. |
| **Atualizar URL com parâmetros de busca** | Realizar uma busca com parâmetros específicos. | **Status:** 200. A URL contém parâmetros `name` e `severity` correspondentes aos filtros aplicados. |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de busca de condições médicas**  
- **Ação:** Acessar a página `/medical-conditions/search` e validar os elementos do formulário.  
- **Validação:**  
   - Campo `Name` está visível.  
   - Campo `Severity` está visível e contém as opções `Low`, `Medium`, `High`.  
   - Botões `Search` e `Clear Filters` estão visíveis.  

### **2. Buscar condições médicas pelo nome**  
- **Ação:** Preencher o campo `Name` com `Asthma` e clicar em `Search`.  
- **Validação:** A lista exibe cartões de condições médicas com o nome correspondente.  

### **3. Buscar condições médicas pela severidade**  
- **Ação:** Selecionar `High` no campo `Severity` e clicar em `Search`.  
- **Validação:** A lista exibe cartões com condições médicas de severidade `High`.  

### **4. Buscar condições médicas com ambos os filtros (nome e severidade)**  
- **Ação:** Preencher `Name` com `Asthma` e selecionar `High` no campo `Severity`.  
- **Validação:** A lista exibe cartões que correspondem a ambos os filtros.  

### **5. Limpar filtros de busca**  
- **Ação:** Preencher filtros, clicar em `Clear Filters`.  
- **Validação:**  
   - Os campos ficam vazios.  
   - A URL não contém parâmetros de busca.  

### **6. Exibir detalhes da condição médica**  
- **Ação:** Clicar em um cartão de condição médica para visualizar detalhes.  
- **Validação:** A modal exibe:  
   - **Nome:** Test Condition  
   - **Severidade:** High  
   - **Descrição:** Test Description  

### **7. Lidar com busca sem resultados**  
- **Ação:** Buscar por uma condição inexistente, como `NonexistentCondition`.  
- **Validação:** A mensagem `Nenhuma condição médica encontrada.` é exibida.  

### **8. Tratar erros do servidor**  
- **Ação:** Simular uma falha no servidor (status `500`) durante a busca.  
- **Validação:** A mensagem `Erro ao buscar condições médicas.` é exibida.  

### **9. Atualizar URL com parâmetros de busca**  
- **Ação:** Buscar com os parâmetros `Name: TestCondition` e `Severity: High`.  
- **Validação:** A URL contém os parâmetros:  
   - `name=TestCondition`  
   - `severity=High`  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões médicas.
- Respostas simuladas do backend foram usadas para validar erros e sucessos controlados.
- Cenários foram testados tanto para sucesso quanto para falhas controladas.
- A interação com a interface foi validada para garantir que todas as funcionalidades operem corretamente.

---

**Última atualização:** `{{date}}`
