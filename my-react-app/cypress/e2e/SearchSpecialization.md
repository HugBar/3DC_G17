# **Documentação de Testes E2E - Buscar Especializações (Frontend)**

**Módulo:** Interface de Busca de Especializações  
**Objetivo:** Validar o funcionamento da interface de busca de especializações, incluindo filtros por nome e descrição, interação com os resultados, tratamento de erros e persistência de filtros após recarregar a página.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de busca de especializações** | Validar que os elementos do formulário (`Name`, `Description`, botões `Search` e `Clear Filters`) estão visíveis. | **Status:** 200. Os elementos estão visíveis e funcionais. |
| **Buscar especialização por nome** | Preencher o campo `Name` e clicar em `Search`. | **Status:** 200. Resultados compatíveis com o nome informado são exibidos. |
| **Buscar especialização por descrição** | Preencher o campo `Description` e clicar em `Search`. | **Status:** 200. Resultados compatíveis com a descrição informada são exibidos. |
| **Limpar filtros de busca** | Preencher os filtros e clicar em `Clear Filters`. | **Status:** 200. Os campos são redefinidos e os parâmetros são removidos da URL. |
| **Exibir detalhes da especialização em uma modal** | Clicar em um cartão de especialização para visualizar seus detalhes. | **Status:** 200. Uma modal exibe informações detalhadas sobre a especialização. |
| **Lidar com busca sem resultados** | Buscar por uma especialização inexistente. | **Status:** 200. **Mensagem:** "No specializations found". |
| **Tratar erros do servidor** | Simular um erro no servidor durante a busca. | **Status:** 500. **Mensagem:** "Error searching specializations". Uma mensagem de erro é exibida. |
| **Manter filtros após recarregar a página** | Preencher os filtros e recarregar a página. | **Status:** 200. Os filtros permanecem aplicados e os parâmetros são preservados na URL. |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de busca de especializações**  
- **Ação:** Acessar a página `/specializations/search` e validar os elementos do formulário.  
- **Validação:**  
   - Campo `Name` está visível.  
   - Campo `Description` está visível.  
   - Botões `Search` e `Clear Filters` estão presentes.  
   - A grade de especializações está carregada corretamente.  

### **2. Buscar especialização por nome**  
- **Ação:** Preencher o campo `Name` com `Cardio` e clicar em `Search`.  
- **Validação:**  
   - A URL contém o parâmetro `name=Cardio`.  
   - Resultados compatíveis com o termo `Cardio` são exibidos.  

### **3. Buscar especialização por descrição**  
- **Ação:** Preencher o campo `Description` com `brain` e clicar em `Search`.  
- **Validação:**  
   - A URL contém o parâmetro `description=brain`.  
   - Resultados compatíveis com a descrição `brain` são exibidos.  

### **4. Limpar filtros de busca**  
- **Ação:** Preencher os campos `Name` e `Description` e clicar em `Clear Filters`.  
- **Validação:**  
   - Os campos ficam vazios.  
   - A URL não contém parâmetros de busca.  

### **5. Exibir detalhes da especialização em uma modal**  
- **Ação:** Clicar em um cartão de especialização.  
- **Validação:**  
   - Uma modal aparece com detalhes da especialização.  
   - A modal contém informações como `Name`, `Description` e outros detalhes relevantes.  
   - O botão `Close` fecha a modal corretamente.  

### **6. Lidar com busca sem resultados**  
- **Ação:** Buscar por uma especialização inexistente, como `NonExistentSpecialization123`.  
- **Validação:**  
   - Uma mensagem `No specializations found` é exibida.  
   - Nenhum cartão de especialização é exibido.  

### **7. Tratar erros do servidor**  
- **Ação:** Simular uma falha no servidor (status `500`) durante a busca.  
- **Validação:**  
   - Uma mensagem `Error searching specializations` é exibida.  
   - Nenhum resultado é exibido na interface.  

### **8. Manter filtros após recarregar a página**  
- **Ação:** Preencher os filtros `Name: Cardio` e `Description: Heart`, clicar em `Search`, e recarregar a página.  
- **Validação:**  
   - Os campos `Name` e `Description` mantêm os valores informados.  
   - A URL contém os parâmetros `name=Cardio` e `description=Heart`.  

---

## **Fluxo do Teste**

1. **Autenticação:** Login válido com token JWT.  
2. **Execução dos testes:** Validação de filtros, interação com modais e tratamento de erros.  
3. **Persistência de dados:** Verificação se os filtros aplicados persistem após recarregar a página.  
4. **Tratamento de falhas:** Validação de mensagens para cenários sem resultados ou falhas no servidor.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- Respostas controladas do backend foram simuladas para validar cenários de falha.
- Foram verificadas as interações tanto com os filtros quanto com a modal de detalhes.

---

**Última atualização:** `{{date}}`
