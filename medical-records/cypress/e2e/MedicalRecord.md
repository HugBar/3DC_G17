# **Documentação de Testes E2E - API de Registros Médicos**

**Módulo:** Gestão de Registros Médicos  
**Objetivo:** Testar endpoints relacionados com a criação, atualização, leitura e busca de registros médicos.  
**Método de teste:** E2E Automatizado (Cypress)  

---

## **Cenários de Teste**

| **Cenário** | **Teste** | **Resultado esperado** |
|------------|-----------|------------------------|
| **Atualizar condições e alergias de um registro existente** | Atualizar as condições e alergias de um registro médico para o paciente `202412000001`. | **Status:** 200 ou 201. O registro é atualizado com as condições e alergias informadas. |
| **Criar um novo registro médico para um paciente** | Criar um registro médico em branco para um novo paciente e atualizar com condições e alergias. | **Status:** 201 (Criação), **Status:** 200 (Atualização). Os dados são corretamente atualizados. |
| **Atualizar registro médico duas vezes consecutivas** | Atualizar duas vezes consecutivas um registro recém-criado com diferentes dados. | **Status:** 200. As duas atualizações são aplicadas corretamente no registro. |
| **Obter registro médico por ID do paciente** | Buscar o registro médico criado para o paciente pelo ID. | **Status:** 200. O registro contém as condições e alergias corretas após a última atualização. |
| **Buscar registros médicos por ID do paciente** | Realizar busca por registros usando apenas o ID do paciente. | **Status:** 200. O registro é retornado corretamente. |
| **Buscar registros médicos com filtro de condição** | Buscar registros usando o filtro `conditionName=Asthma`. | **Status:** 200. A condição `Asthma` com severidade `High` está presente no resultado. |
| **Buscar registros médicos com filtro de alergia** | Buscar registros usando o filtro `allergyName=Dust`. | **Status:** 200. A alergia `Dust` com severidade `Medium` está presente no resultado. |
| **Buscar registros com múltiplos filtros** | Buscar registros usando os filtros `conditionName=Asthma` e `allergyName=Dust`. | **Status:** 200. A condição `Asthma` e a alergia `Dust` são retornadas corretamente. |
| **Buscar registro médico com ID inexistente** | Buscar registros com um ID de paciente inexistente (`nonexistent`). | **Status:** 404. **Mensagem:** "Medical record not found". |

---

## **Observações Finais**
- Testes foram realizados com autenticação válida usando um token JWT de um usuário com permissões médicas.
- IDs de pacientes dinâmicos foram usados (`Date.now()`) para garantir exclusividade nos testes.
- Validação realizada com base nos códigos de status HTTP e dados retornados pela API.
- Diferentes filtros foram aplicados para validar a robustez do endpoint de busca.

---

**Última atualização:** `{{date}}`
