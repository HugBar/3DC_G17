# **Documentação de Testes E2E - API de Condições Médicas**

**Módulo:** Gestão de Condições Médicas  
**Objetivo:** Testar endpoints relacionados com a criação, atualização, pesquisa e associação de condições médicas aos registros de pacientes.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Adicionar nova condição médica ao catálogo** | Adicionar uma nova condição médica com nome único (`TestCondition_${Date.now()}`). | **Status:** 201. **Mensagem:** "Medical condition added successfully". A condição é criada com sucesso. |
| **Criar condição médica duplicada** | Tentar criar uma condição médica com o nome `Hypertension`. | **Status:** 409. **Mensagem:** "Medical condition already exists". |
| **Buscar condição médica pelo nome** | Buscar condição médica usando o parâmetro `name=Hypertension`. | **Status:** 200. A resposta contém uma lista com pelo menos uma condição com o nome `Hypertension`. |
| **Buscar condições médicas por severidade** | Buscar condições médicas usando o parâmetro `severity=High`. | **Status:** 200. A resposta contém uma lista onde todas as condições têm severidade `High`. |
| **Buscar todas as condições médicas** | Buscar todas as condições médicas disponíveis no catálogo. | **Status:** 200. A resposta contém uma lista com pelo menos uma condição médica. |
| **Submeter dados inválidos para condição médica** | Tentar adicionar uma condição médica com dados inválidos (`name: '', severity: '', description: ''`). | **Status:** 500. **Mensagem:** "Internal server error". |
| **Buscar condição médica inexistente pelo nome** | Buscar uma condição médica com um nome inexistente (`NonExistent_${Date.now()}`). | **Status:** 200. A resposta contém uma lista vazia. |
| **Adicionar condição médica ao registro de um paciente** | Adicionar a condição médica `Diabetes` ao registro do paciente `TestPatient123`. | **Status:** 201 (Sucesso) ou **404 (Paciente não encontrado)**. **Mensagem:** "Medical condition added successfully" (em caso de sucesso). |

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões de administrador.
- Nomes exclusivos foram gerados dinamicamente (`Date.now()`) para evitar conflitos em testes consecutivos.
- Testes de busca validaram corretamente filtros por nome e severidade.
- A robustez dos endpoints foi verificada para dados inválidos e cenários de erro.

---

**Última atualização:** `{{date}}`
