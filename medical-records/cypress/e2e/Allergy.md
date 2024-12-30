# **Documentação de Testes E2E - API de Alergias**

**Módulo:** Gestão de Alergias  
**Objetivo:** Testar endpoints relacionados com a criação, atualização, pesquisa e exclusão de alergias no sistema.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Adicionar uma nova alergia ao catálogo** | Adicionar uma nova alergia com nome exclusivo (`TestAllergen_${Date.now()}`). | **Status:** 201. **Mensagem:** "Allergy added successfully". A alergia é criada com sucesso. |
| **Criar alergia duplicada** | Tentar criar uma alergia com o nome `Peanuts`. | **Status:** 409. **Mensagem:** "Allergy already exists". |
| **Buscar alergias por alérgeno** | Buscar alergias usando o parâmetro `allergen=Peanuts`. | **Status:** 200. A resposta contém uma lista onde todas as alergias possuem o alérgeno `Peanuts`. |
| **Buscar alergias por severidade** | Buscar alergias usando o parâmetro `severity=High`. | **Status:** 200. A resposta contém uma lista onde todas as alergias possuem severidade `High`. |
| **Buscar todas as alergias** | Buscar todas as alergias disponíveis no catálogo. | **Status:** 200. A resposta contém uma lista com pelo menos uma alergia registrada. |
| **Submeter dados inválidos para alergia** | Tentar adicionar uma alergia com dados inválidos (`allergen: '', severity: '', diagnosedDate: '', notes: ''`). | **Status:** 500. **Mensagem:** "Internal server error". |
| **Buscar alergia inexistente pelo alérgeno** | Buscar uma alergia com um nome inexistente (`NonExistent_${Date.now()}`). | **Status:** 200. A resposta contém uma lista vazia. |
| **Excluir alergia específica** | Excluir a alergia criada no teste com o nome exclusivo (`TestAllergen_${Date.now()}`). | **Status:** 200. **Mensagem:** "Allergy deleted successfully". |

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões administrativas.
- Nomes exclusivos foram gerados dinamicamente (`Date.now()`) para garantir que não haja conflito entre os testes.
- Cada cenário foi validado para confirmar tanto sucessos quanto falhas esperadas.
- Testes de busca validaram corretamente filtros por alérgeno e severidade.

---

**Última atualização:** `{{date}}`
