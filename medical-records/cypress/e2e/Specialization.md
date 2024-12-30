# **Documentação de Testes E2E - API de Especializações**

**Módulo:** Gestão de Especializações  
**Objetivo:** Testar endpoints relacionados com a criação, leitura, atualização e exclusão de especializações.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Criação de especialização com nome único** | Criar uma nova especialização com um nome exclusivo (`TestSpecialization_${Date.now()}`). | **Status:** 201. O corpo da resposta contém o nome e descrição corretos da especialização criada. |
| **Criação de especialização duplicada** | Criar uma especialização com o nome `Cardiology`. | **Status:** 409. **Mensagem:** "Specialization already exists". |
| **Obter todas as especializações** | Obter a lista de todas as especializações existentes. | **Status:** 200. A resposta contém uma lista de especializações com pelo menos 3 itens, incluindo `Cardiology` e `Neurology`. |
| **Validação de erros na criação de especialização** | Enviar uma especialização com campos vazios (`name: '', description: ''`). | **Status:** 500. **Mensagem:** "Internal server error". |
| **Obter especialização por ID válido** | Buscar uma especialização recém-criada por seu ID. | **Status:** 200. A resposta contém os detalhes corretos da especialização. |
| **Obter especialização por ID inválido** | Buscar uma especialização com ID inexistente (`507f1f77bcf86cd799439011`). | **Status:** 404. **Mensagem:** "Specialization not found". |
| **Atualizar uma especialização existente** | Atualizar o nome e a descrição de uma especialização existente. | **Status:** 200. **Mensagem:** "Specialization updated successfully". Os dados são atualizados corretamente. |
| **Atualizar especialização com nome duplicado** | Tentar atualizar uma especialização para o nome `Cardiology`. | **Status:** 409. **Mensagem:** "Specialization with this name already exists". |
| **Atualizar especialização inexistente** | Tentar atualizar uma especialização com ID inexistente. | **Status:** 404. **Mensagem:** "Specialization not found". |
| **Excluir uma especialização existente** | Excluir uma especialização recém-criada. | **Status:** 200. **Mensagem:** "Specialization deleted successfully". |
| **Excluir especialização inexistente** | Tentar excluir uma especialização com ID inexistente. | **Status:** 404. **Mensagem:** "Specialization not found". |
| **Verificar remoção da lista após exclusão** | Excluir uma especialização e verificar se ela foi removida da lista. | **Status:** 200. A especialização excluída não aparece mais na lista. |

---

### **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT.
- Dados dinâmicos (`Date.now()`) foram usados para garantir nomes únicos durante os testes.
- O comportamento esperado foi validado com base nos códigos de status HTTP e mensagens retornadas pela API.

