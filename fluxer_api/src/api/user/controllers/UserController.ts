// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoApp} from '../../types/HonoEnv';
import {EntranceSoundController} from '../entrance_sound/EntranceSoundController';
import {EntranceSoundPlayController} from '../entrance_sound/EntranceSoundPlayController';
import {UserAccountController} from './UserAccountController';
import {UserAuthController} from './UserAuthController';
import {UserChannelController} from './UserChannelController';
import {UserContentController} from './UserContentController';
import {UserRelationshipController} from './UserRelationshipController';

export function UserController(app: HonoApp) {
	UserAccountController(app);
	UserAuthController(app);
	UserRelationshipController(app);
	UserChannelController(app);
	UserContentController(app);
	EntranceSoundController(app);
	EntranceSoundPlayController(app);
}
