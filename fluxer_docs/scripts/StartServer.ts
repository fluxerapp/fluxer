// SPDX-License-Identifier: AGPL-3.0-or-later

import {runAstroServer} from '@/server/AstroServer';
import {docsListenHost, docsListenPort, docsPublicEndpoint} from '@/server/DocsConfig';

await runAstroServer({
	entrypoint: new URL('../dist/server/entry.mjs', import.meta.url),
	initialize: null,
	listenHost: docsListenHost(),
	listenPort: docsListenPort(),
	publicEndpoint: docsPublicEndpoint(),
	readiness: null,
	requestLocals: null,
	shutdown: null,
});
