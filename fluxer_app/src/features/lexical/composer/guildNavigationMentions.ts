// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ComposerInsertPayload} from '@app/features/lexical/composer/composerOffsets';
import {ROLES_PAGE_MENTION_TOKEN} from '@fluxer/constants/src/GuildConstants';

export const ROLES_NAVIGATION_ID = 'roles';

export function createRolesNavigationPayload(display: string): ComposerInsertPayload {
	return {kind: 'mention', mentionType: 'navigation', id: ROLES_NAVIGATION_ID, display, wire: ROLES_PAGE_MENTION_TOKEN};
}
