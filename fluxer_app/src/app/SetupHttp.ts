// SPDX-License-Identifier: AGPL-3.0-or-later

import SudoPrompt from '@app/features/auth/state/SudoPrompt';
import SessionManager from '@app/features/platform/state/AuthSession';
import {http} from '@app/features/platform/transport/RestTransport';

export function setupHttp(): void {
	http.installAuth(() => SessionManager.token);
	SudoPrompt.init();
}
