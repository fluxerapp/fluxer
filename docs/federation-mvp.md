# Federación en Fluxer: diseño del MVP

Estado: borrador de diseño
Rama: `feat/federation-mvp`
Objetivo: que dos instancias independientes de Fluxer intercambien mensajes de texto
con entrega duradera, incluso si la instancia receptora está desconectada.

Este documento fija las decisiones antes de tocar código. Si una decisión cambia,
se actualiza aquí primero.

---

## 1. Alcance

### Dentro del MVP

- Mensajes de texto en canales de guild.
- Dos instancias con confianza explícita (allowlist de pares).
- Eventos firmados, persistentes y reintentables.
- Confirmación de entrega (ACK) tras escritura durable en el receptor.
- Idempotencia: reenviar un evento no crea duplicados.
- Tombstones para edición y borrado.
- Representación local de entidades remotas (guild, canal, usuario, mensaje).

### Fuera del MVP

- P2P entre clientes y sincronización en segundo plano en móviles.
- Emojis, stickers, archivos y adjuntos.
- Presencia, voz y vídeo.
- Descubrimiento global de instancias.
- Migración de cuentas entre instancias.
- Cifrado extremo a extremo.
- Recuperación de cuentas y antiabuso a gran escala.

Voz y vídeo siguen usando la arquitectura actual (WebRTC/SFU). No se convierten en
malla P2P.

---

## 2. Decisiones cerradas

| Tema | Decisión v1 | Motivo |
| --- | --- | --- |
| Unidad federada | Mensajes de texto | Es el caso con menos dependencias de medios y el más fácil de verificar |
| Propiedad del dato | Cada instancia es dueña de sus usuarios, guilds y mensajes | Evita escritura distribuida y conflictos de autoridad |
| Copia remota | El receptor guarda una copia/referencia para renderizar | La fuente de verdad sigue siendo la instancia de origen |
| Identidad de usuario | `usuario@dominio-de-instancia` en eventos | Namespacing sin cambiar los IDs internos |
| Identidad de instancia | Par de claves Ed25519 por instancia | Firmar eventos y autenticar servidores sin sesiones |
| Descubrimiento | Allowlist explícita de pares y claves fijadas | Un prototipo no necesita descubrimiento abierto |
| Transporte | HTTPS `POST` de eventos entre instancias | Simple, depurable y compatible con cualquier host |
| Autenticación | Firma Ed25519 sobre el evento canónico | No se expone NATS ni sesiones internas |
| Entrega | At-least-once + aplicación idempotente | Tolerar caídas sin perder eventos |
| Confirmación | ACK solo después de escritura durable | Evita borrar del outbox antes de tiempo |
| Fuera de línea | El emisor reintenta; el receptor conserva en inbox | No se borra un evento porque nadie esté conectado |
| Retención | Política explícita y configurable (30 días por defecto) | Un relay "temporal" que borra al entregar pierde datos |
| Ventana de actividad | Solo para avisar/sincronizar, nunca para bloquear el envío local | El usuario puede seguir escribiendo en local |
| Borrado | Evento tombstone; no se promete borrado físico remoto | En distribuido no se puede garantizar |
| Auth y sesiones | Se quedan como están, por instancia | Alta coherencia y seguridad |
| IDs remotos | Clave compuesta `(origin, remote_id)` + ID local sustituto | Evitar colisiones de snowflakes entre instancias |
| Medios y llamadas | Sin cambios | No condicionar el chat federado a la malla de medios |

Ninguna función central (login, cuotas, revocación) se elimina en esta etapa.

---

## 3. Flujo actual del mensaje

Ruta de escritura, toda en `fluxer_api`:

1. `POST /channels/:channel_id/messages` en
   `fluxer_api/src/api/channel/controllers/MessageController.ts:205`.
2. `MessageRequestService.sendMessage` en
   `fluxer_api/src/api/channel/services/message/MessageRequestService.ts:175`.
3. `MessageSendService.sendMessage` en
   `fluxer_api/src/api/channel/services/message/MessageSendService.ts:619`.
4. Se genera el snowflake del mensaje en `MessageSendService.ts:266`.
5. Se persiste con `persistenceService.createMessage` en `MessageSendService.ts:852`.
6. Se despacha por el gateway en `MessageSendService.ts:931`.
7. Se emite actividad local en `MessageSendService.ts:954`.

Ruta de lectura/formateo:

- `MessageResponseDataService` pide respuestas al sujeto NATS `svc.messages`
  (`MessageResponseDataService.ts:21`).
- El servicio Rust `fluxer_messages` responde con `MessagesRouter`
  (`fluxer_messages/src/router_impl.rs:26`).

Los mensajes se guardan en tablas Cassandra/Scylla definidas en
`fluxer_api/src/api/Tables.ts` y usadas por
`fluxer_api/src/api/channel/repositories/message/MessageDataRepository.ts`.
El esquema de desarrollo se regenera en `tools/dev/cassandra_target_schema.json`.

**Importante:** NATS y el gateway son buses internos. No se exponen a instancias
externas. La federación es un protocolo HTTP entre servidores.

---

## 4. Arquitectura propuesta (MVP en TypeScript)

```
Instancia A                                            Instancia B
┌──────────────────────────┐      HTTPS firmado      ┌──────────────────────────┐
│ MessageSendService       │                         │ POST /federation/v1/events│
│   └─ commit mensaje      │                         │   ├─ verificar firma      │
│   └─ insert outbox       │ ──────────────────────▶ │   ├─ dedupe por event_id  │
│ FederationOutbox worker  │ ◀────────────────────── │   ├─ persistir inbox      │
│   └─ reintento + ACK     │        ACK 202          │   └─ aplicar evento       │
└──────────────────────────┘                         │ FederationApplyService    │
                                                     │   └─ gateway interno      │
                                                     └──────────────────────────┘
```

Se implementa primero en `fluxer_api` porque la escritura ya vive ahí y el servidor
HTTP existe. Si más adelante hace falta rendimiento o aislamiento, se extrae a un
crate Rust `fluxer_federation` que hable con la API por un contrato interno.

### Estructura de archivos

```
fluxer_api/src/api/federation/
  FederationConfig.ts          # env, allowlist, retención, timeouts
  FederationCrypto.ts          # Ed25519, canonicalización, verificación
  FederationEvent.ts           # tipos y validación del envelope
  FederationController.ts      # POST /federation/v1/events y /handshake
  FederationOutbox.ts          # worker de envío y reintentos
  FederationInbox.ts           # persistencia, dedupe y cursores
  FederationPeerRepository.ts  # pares, claves y estado
  FederationApplyService.ts    # validación de autor y aplicación local
  FederationRemoteRepository.ts# mapeo (origin, remote_id) -> id local
```

---

## 5. Puntos de integración

### 5.1 Emisión (salida)

En `MessageSendService.sendMessage`, **después** de `createMessage`
(`MessageSendService.ts:852`) y **sin bloquear** el envío local:

- Insertar el evento `message.create` en el outbox de federación.
- Ideal: en la misma transacción/operación durable que el mensaje.
- Si no es posible en el almacenamiento actual, insertar inmediatamente después y
  añadir un barrido de reconciliación que detecte mensajes sin evento.
- El fallo de federación nunca debe hacer fallar la respuesta al usuario. Debe
  emitir métrica y log, y quedar `pending`.

No enviar antes del commit local.

### 5.2 Recepción (entrada)

Ruta nueva `POST /federation/v1/events`, **fuera** de `LoginRequired`
(se autentica por firma, no por sesión):

1. Limitar tamaño y leer el envelope.
2. Buscar el par y su clave por `origin` + `key_id`.
3. Verificar firma, `occurred_at` dentro de una ventana y `seq`/replay.
4. Persistir en inbox con unicidad `(origin, event_id)`.
5. Responder `202` solo tras escritura durable.
6. Aplicar de forma asíncrona:
   - validar autor y pertenencia,
   - mapear entidades remotas a IDs locales sustitutos,
   - guardar el mensaje remoto,
   - despachar al gateway local con `gatewayService`.

Duplicados devuelven `200/202` sin reaplicar. Firma inválida devuelve `401/422` y
no se persiste como aplicable.

### 5.3 Aplicación y gateway

El gateway local se reutiliza para que la interfaz reciba el mensaje remoto.
No se conecta el bus interno a terceros.

### 5.4 Configuración

Añadir a `config/env/development.env` y a la documentación del operador:

```
FLUXER_FEDERATION_ENABLED=false
FLUXER_FEDERATION_INSTANCE_ID=localhost
FLUXER_FEDERATION_PRIVATE_KEY=<base64 ed25519>
FLUXER_FEDERATION_KEY_ID=<id corto>
FLUXER_FEDERATION_PEERS=<json allowlist>
FLUXER_FEDERATION_TEST_CHANNEL_ID=<canal local de prueba>
FLUXER_FEDERATION_TIMEOUT_MS=10000
FLUXER_FEDERATION_MAX_ATTEMPTS=10
FLUXER_FEDERATION_RETRY_BASE_MS=1000
FLUXER_FEDERATION_RETRY_MAX_MS=3600000
```

Cada entrada de `FLUXER_FEDERATION_PEERS` incluye `instance_id`, `base_url` y
`public_key` (Ed25519 en base64url).

Nunca hardcodear claves. En desarrollo, claves de prueba fuera de git.

---

## 6. Protocolo de eventos v1

Envelope JSON:

```json
{
  "v": 1,
  "event_id": "01JAB...",
  "origin": "chat.example.org",
  "seq": 1234,
  "type": "message.create",
  "occurred_at": "2026-11-24T17:00:00.000Z",
  "payload": {},
  "payload_hash": "sha256:...",
  "prev_hash": "sha256:...",
  "signature": "ed25519:..."
}
```

Reglas:

- `event_id`: ULID, único por origen.
- `seq`: entero monotónico por origen; sirve de cursor y detección de huecos.
- Canonicalización: JSON canónico (RFC 8785 / JCS).
- Firma: Ed25519 sobre los bytes canónicos del envelope sin `signature`.
- `payload_hash`: SHA-256 del payload canónico.
- `prev_hash`: hash del evento anterior del mismo origen (cadena de auditoría).
- Cabeceras: `X-Fluxer-Key-Id` y `X-Fluxer-Signature`.
- Reloj: rechazar desviaciones mayores a la ventana configurada.
- Replay: unicidad `(origin, event_id)` + `seq` no retrocede por par.

### Carga útil `message.create`

```json
{
  "guild":   {"origin": "chat.example.org", "id": "123"},
  "channel": {"origin": "chat.example.org", "id": "456"},
  "message": {
    "origin": "chat.example.org",
    "id": "789",
    "author": {"handle": "ana@chat.example.org", "display_name": "Ana"},
    "content": "hola",
    "created_at": "2026-11-24T17:00:00.000Z",
    "reply_to": null
  }
}
```

`message.edit` y `message.delete` usan la misma dirección y añaden
`edited_at` o un tombstone. En v1, el tombstone gana sobre el contenido anterior.

### Estados

- Outbox: `pending` → `sending` → `acked` | `failed` (reintento) | `expired`.
- Inbox: `received` → `applied` | `rejected` (con motivo auditable).

Reintentos con backoff exponencial y jitter. Límite por par para evitar abuso.

---

## 7. Almacenamiento

Añadir tablas al esquema Cassandra/Scylla en `fluxer_api/src/api/Tables.ts` y
regenerar `tools/dev/cassandra_target_schema.json`:

```text
federation_peers(
  instance_id text PRIMARY KEY,
  public_key text,
  key_id text,
  status text,
  created_at timestamp,
  last_seen_at timestamp
)

federation_outbox(
  event_id text PRIMARY KEY,
  origin text,
  seq bigint,
  type text,
  payload text,
  created_at timestamp,
  next_attempt_at timestamp,
  attempts int,
  acked_at timestamp
)

federation_inbox(
  origin text,
  event_id text,
  received_at timestamp,
  applied_at timestamp,
  payload_hash text,
  status text,
  PRIMARY KEY ((origin), event_id)
)

federation_remote_entities(
  origin text,
  entity_type text,
  remote_id text,
  local_id text,
  first_seen_at timestamp,
  PRIMARY KEY ((origin, entity_type), remote_id)
)
```

Alternativa aceptable para el primer prototipo: guardar el inbox/outbox en
Postgres o KV si acelera la validación, pero el diseño final debe vivir junto al
resto de datos de mensajes.

---

## 8. Identificadores remotos

Los snowflakes actuales suponen un único espacio de IDs. Dos instancias pueden
generar el mismo número. Por eso:

- No insertar `remote_id` directamente como si fuera un ID local.
- Usar clave compuesta `(origin, entity_type, remote_id)`.
- Generar un ID local sustituto para referencias internas y para el gateway.
- Mantener el `origin` visible en la respuesta para pintar la procedencia.
- Al renderizar, mostrar `usuario@dominio` cuando el autor sea remoto.

Esto toca `@fluxer/schema` y los clientes: es un cambio transversal que conviene
hacer explícito desde el principio, aunque en el primer prototipo se pueda simular
con datos de prueba.

---

## 9. Criterios de aceptación del MVP

La primera versión está lista cuando se demuestra, entre dos instancias A y B:

- [ ] Un mensaje creado en A llega a B y B lo persiste.
- [ ] B valida que el evento viene de un par reconocido y con firma correcta.
- [ ] B responde ACK y A marca el evento como entregado.
- [ ] Si B está apagada, el evento queda `pending` y se reintenta al volver.
- [ ] Reenviar el mismo evento no crea duplicados.
- [ ] Una firma inválida se rechaza y no se aplica.
- [ ] Un evento con `event_id` repetido pero contenido distinto se rechaza.
- [ ] Un evento fuera de la ventana de reloj se rechaza.
- [ ] Un borrado se propaga como tombstone.
- [ ] Un usuario desconectado de B recibe los eventos al reconectar (inbox persistente).
- [ ] El envío local en A sigue funcionando si la federación falla.

Matriz de pruebas mínima: entrega normal, receptor caído, duplicado, firma mala,
evento grande, reloj desviado, clave revocada, tombstone.

---

## 10. Plan por etapas

1. **Etapa 0 — este documento y el diagrama.** Hecha.
2. **Etapa 1 — criptografía y envelope.** Hecha en
   `fluxer_api/src/api/federation/FederationCrypto.ts` y
   `fluxer_api/src/api/federation/FederationEvent.ts`, con 22 pruebas en los
   archivos `*.test.ts` del mismo directorio. Incluye claves Ed25519, canonicalización
   JCS, envelope firmado, hash de payload, cadena `prev_hash`, ventana de reloj y
   cursor de secuencia.
3. **Etapa 2 — outbox de salida.** Hecha en
   `fluxer_api/src/api/federation/FederationConfig.ts`, `FederationOutboxRepository.ts`,
   `FederationTransport.ts`, `FederationOutboxService.ts`, `DeliverFederationOutbox.ts` y sus tests.
   Integra outbox durable en Cassandra (`federation_outbox`), enganche en `MessageSendService`,
   worker de drenado por cron (lane `batch`, cada 5s), transporte HTTP POST y reintentos con backoff.
4. **Etapa 3 — inbox de entrada.** Hecha en `FederationInboxRepository.ts`,
   `FederationIncomingService.ts`, `FederationInboxApplicationService.ts` y `FederationController.ts`.
   Incluye ruta `POST /federation/v1/events`, verificación Ed25519 con clave fijada por par,
   dedupe `(origin, event_id)`, persistencia durable antes del `202`, y despacho de
   `MESSAGE_CREATE` al canal de prueba configurado (`FLUXER_FEDERATION_TEST_CHANNEL_ID`).
5. **Etapa 4 — robustez.** Hecha la base: cursor de secuencia por origen en KV,
   dedupe y replay, límite de tamaño y longitud, métricas Prometheus de entrada,
   aplicación y outbox, y worker de reconciliación (`reconcileFederationInbox`,
   cada 30s) para eventos que quedaron en `received` tras una caída. La indexación
   de pendientes usa un barrido acotado por origen; una tabla índice dedicada queda
   como mejora si el volumen crece.
6. **Etapa 5 — después.** Emojis/recursos por hash, descubrimiento, autoridad de
   cuentas, medios.

No avanzar de etapa sin que la anterior tenga pruebas que fallen si se rompe.

---

## 11. Riesgos y decisiones abiertas

- **Outbox transaccional:** confirmar si el almacenamiento actual permite insertar
  mensaje y evento en una sola operación. Si no, definir reconciliación.
- **Mapeo de entidades remotas:** cómo se crea y mantiene un guild/canal remoto
  espejo y quién autoriza la pertenencia.
- **Renderizado en clientes:** añadir `origin` a las respuestas y mostrar
  procedencia; evaluar impacto en `@fluxer/schema` y apps.
- **Retención:** 30 días es una propuesta; debe ser política explícita del operador.
- **Confianza:** allowlist cerrada en v1; federación abierta requiere reputación,
  moderación y antiabuso.
- **Licencia y marca:** el proyecto es AGPL-3.0-or-later y el nombre/logotipos
  tienen política aparte. Revisar antes de distribuir una versión derivada.
- **Privacidad:** retener contenido remoto tiene implicaciones de borrado y
  cumplimiento; documentar la política.

---

## 12. Primeros comandos

```sh
git checkout -b feat/federation-mvp   # ya creada
pnpm install
pnpm dev:infra:start                   # Postgres, Valkey, NATS, etc.
pnpm dev:bootstrap                     # preparar datos/entorno local
pnpm dev                               # levantar la instancia
```

Antes de implementar, confirmar que una instancia local arranca y que se puede
enviar un mensaje. Después, crear la segunda instancia con otro puerto y otro
`FLUXER_FEDERATION_INSTANCE_ID`.

Para ejecutar solo las pruebas de la Etapa 1:

```sh
pnpm --filter fluxer_api exec vitest run src/api/federation
```
