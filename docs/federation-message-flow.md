# Recorrido de un mensaje federado

Diagrama del MVP descrito en `federation-mvp.md`. Rama: `feat/federation-mvp`.

---

## 1. Componentes

```mermaid
flowchart LR
  subgraph A["Instancia A (origen)"]
    U["Cliente de usuario A"]
    API_A["fluxer_api\nMessageController"]
    SEND["MessageSendService\n(commit + outbox)"]
    OB["Outbox\nfederation_outbox"]
    W["Outbox worker\n(firma + reintentos)"]
    GW_A["Gateway local"]
    PEER_A["federation_peers"]
  end

  subgraph B["Instancia B (destino)"]
    EP["POST /federation/v1/events\nFederationController"]
    CRY["Verificación Ed25519\n+ replay + reloj"]
    IN["Inbox\nfederation_inbox"]
    APPLY["FederationApplyService\n(author + membership)"]
    MAP["Remote entities\n(origin, remote_id) → local_id"]
    GW_B["Gateway local"]
    U_B["Clientes de B"]
  end

  U --> API_A --> SEND
  SEND --> GW_A
  SEND -->|"misma operación durable\n(o justo después)"| OB
  PEER_A --> W
  OB --> W
  W -->|"HTTPS firmado\nmessage.create"| EP
  EP --> CRY --> IN
  EP -->|"202 ACK\ntras escritura durable"| W
  IN --> APPLY --> MAP
  APPLY --> GW_B --> U_B
```

---

## 2. Secuencia: entrega normal

```mermaid
sequenceDiagram
  autonumber
  participant C as Cliente A
  participant A as fluxer_api A
  participant OA as Outbox A
  participant W as Worker A
  participant B as fluxer_api B
  participant IB as Inbox B
  participant G as Gateway B

  C->>A: POST /channels/:id/messages
  A->>A: validar permisos y contenido
  A->>A: crear mensaje (snowflake + persistencia)
  A->>OA: insertar message.create (pending)
  A->>C: 200 MessageResponse
  A-->>G: dispatch local (no espera a B)
  W->>B: POST /federation/v1/events (firmado, seq=N)
  B->>B: verificar firma, key_id, reloj, replay
  B->>IB: persistir (origin, event_id)
  B-->>W: 202 Accepted
  W->>OA: marcar acked
  B->>B: aplicar evento y mapear autor remoto
  B->>G: despachar a clientes de B
```

El envío local no espera a B. La federación es asíncrona y reintentable.

---

## 3. Secuencia: destino apagado

```mermaid
sequenceDiagram
  autonumber
  participant A as fluxer_api A
  participant OA as Outbox A
  participant B as Instancia B (caída)

  A->>OA: message.create (pending)
  OA->>B: POST /federation/v1/events
  B--xA: error de conexión / timeout
  OA->>OA: attempts++, backoff exponencial + jitter
  Note over OA: el evento NO se borra
  B->>B: (más tarde) arranca
  OA->>B: reintento del mismo event_id
  B->>B: dedupe por (origin, event_id)
  B-->>OA: 202 ACK
  OA->>OA: acked
```

La instancia origen conserva el evento hasta ACK o expiración explícita.

---

## 4. Secuencia: receptor con usuarios desconectados

```mermaid
sequenceDiagram
  autonumber
  participant W as Worker A
  participant B as fluxer_api B
  participant IB as Inbox B
  participant U as Usuario de B (offline)

  W->>B: POST mensaje
  B->>IB: persistir evento
  B-->>W: 202 ACK
  Note over B,U: el usuario de B no está conectado
  Note over IB: el evento permanece en inbox
  U->>B: reconecta / sincroniza cursor
  B->>IB: leer eventos posteriores al cursor
  B->>U: entregar mensajes pendientes
```

El inbox no se vacía por no haber usuarios conectados. La retención es una
política explícita (30 días por defecto), no "hasta que se entregue a los activos".

---

## 5. Secuencia: borrado con tombstone

```mermaid
sequenceDiagram
  autonumber
  participant A as Instancia A (origen)
  participant B as Instancia B
  participant U as Usuario de B

  A->>B: message.delete (evento firmado)
  B->>B: verificar y persistir tombstone
  B->>B: ocultar/marcar mensaje remoto
  B-->>A: 202 ACK
  Note over B: no se promete borrado físico\nen copias ya replicadas
  B->>U: actualización (mensaje eliminado)
```

---

## 6. Estados

### Outbox

```mermaid
stateDiagram-v2
  [*] --> pending
  pending --> sending: worker toma el evento
  sending --> acked: 202 del destino
  sending --> pending: error/timeout (backoff)
  sending --> rejected: 4xx no recuperable
  pending --> expired: supera retención
  acked --> [*]
  rejected --> [*]
  expired --> [*]
```

### Inbox

```mermaid
stateDiagram-v2
  [*] --> received: firma válida + persistido
  received --> applied: autor/membresía válidos
  received --> rejected: autor/membresía inválidos
  applied --> [*]
  rejected --> [*]
```

---

## 7. Handshake entre instancias

```mermaid
sequenceDiagram
  autonumber
  participant A as Instancia A
  participant B as Instancia B

  A->>B: GET /federation/v1/identity
  B-->>A: {instance_id, key_id, public_key, protocol_version}
  A->>A: comparar con allowlist y clave fijada
  Note over A,B: si no coincide, no se envía nada
```

En el MVP el handshake es opcional: la allowlist y las claves fijadas bastan.
El endpoint de identidad sirve para rotación de claves y diagnóstico.
