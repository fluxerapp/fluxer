// SPDX-License-Identifier: AGPL-3.0-or-later

import {checkAstroServerHealth} from '../src/server/AstroServer';
import {docsListenHost, docsListenPort} from '../src/server/DocsConfig';

await checkAstroServerHealth({
	listenHost: docsListenHost(),
	listenPort: docsListenPort(),
	timeoutMs: 4_000,
});
