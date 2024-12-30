# **Documentação de Testes E2E - Buscar Alergias (Frontend)**

**Módulo:** Interface de Busca de Alergias  
**Objetivo:** Validar o funcionamento da interface de busca de alergias, incluindo filtros, manipulação de resultados, tratamento de erros e interação com os resultados da busca.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Exibir corretamente o formulário de busca de alergias** | Validar que os elementos do formulário estão visíveis (`Allergen Name`, `Severity`, botões `Search` e `Clear Filters`). | **Status:** 200. Os elementos do formulário estão visíveis e funcionais. |
| **Aplicar filtros de busca corretamente** | Preencher os campos `Allergen Name` e `Severity`, clicar no botão `Search` e verificar a URL. | **Status:** 200. A URL contém parâmetros `allergen` e `severity` correspondentes aos valores informados. |
| **Limpar filtros de busca** | Preencher os filtros, clicar no botão `Clear Filters` e validar que os filtros foram limpos. | **Status:** 200. Os campos são redefinidos e os parâmetros são removidos da URL. |
| **Exibir mensagem de "Nenhum resultado encontrado"** | Realizar uma busca com um alérgeno inexistente. | **Status:** 200. Uma mensagem `No allergies found` é exibida. |
| **Atualizar URL com parâmetros de busca** | Realizar uma busca com parâmetros específicos e validar se estão refletidos na URL. | **Status:** 200. A URL contém os parâmetros `allergen` e `severity`. |
| **Tratar erros do servidor** | Simular um erro de servidor (status `500`) durante a busca. | **Status:** 500. **Mensagem:** "Error searching allergies". Uma mensagem de erro é exibida. |
| **Interagir com um cartão de resultado** | Realizar uma busca com resultados válidos e clicar em um cartão de alergia para visualizar detalhes. | **Status:** 200. A tela de detalhes é exibida com informações sobre a alergia selecionada. |

---

## **Detalhes dos Testes**

### **1. Exibir corretamente o formulário de busca de alergias**  
- **Ação:** Acessar a página `/allergies/search` e validar a presença dos elementos do formulário.  
- **Validação:**  
   - Campo `Allergen Name` está visível.  
   - Campo `Severity` (`High`, `Medium`, `Low`) está disponível.  
   - Botões `Search` e `Clear Filters` estão presentes.  

### **2. Aplicar filtros de busca corretamente**  
- **Ação:** Preencher os campos `Allergen Name` e `Severity`, clicar em `Search`.  
- **Validação:** A URL contém parâmetros `allergen` e `severity` com os valores fornecidos.  

### **3. Limpar filtros de busca**  
- **Ação:** Preencher os filtros e clicar em `Clear Filters`.  
- **Validação:**  
   - Os campos ficam vazios.  
   - A URL não contém parâmetros de busca.  

### **4. Exibir mensagem de "Nenhum resultado encontrado"**  
- **Ação:** Buscar por um alérgeno inexistente.  
- **Validação:** A mensagem `No allergies found` é exibida.  

### **5. Atualizar URL com parâmetros de busca**  
- **Ação:** Realizar uma busca usando valores específicos nos campos e clicar em `Search`.  
- **Validação:** A URL contém os parâmetros `allergen` e `severity`.  

### **6. Tratar erros do servidor**  
- **Ação:** Simular uma falha no servidor ao realizar uma busca.  
- **Validação:** Uma mensagem de erro `Error searching allergies` é exibida.  

### **7. Interagir com um cartão de resultado**  
- **Ação:** Buscar por alergias existentes, clicar em um cartão de alergia para visualizar detalhes.  
- **Validação:** A tela de detalhes é exibida com informações sobre a alergia selecionada.  

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões médicas.
- Interceptações (`cy.intercept`) foram usadas para simular respostas controladas do backend.
- Cada cenário validou tanto sucessos quanto falhas esperadas.
- A interação com os resultados foi verificada para garantir que os detalhes fossem exibidos corretamente.

---

**Última atualização:** `{{date}}`
