/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, reaction} from 'mobx';
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';

const logger = new Logger('MemberListStore');

const getInitialWidth = (): number => window.innerWidth;

class MemberListStore {
	isMembersOpen = getInitialWidth() >= 1024;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'MemberListStore', ['isMembersOpen']);
	}

	toggleMembers(): void {
		this.isMembersOpen = !this.isMembersOpen;
		logger.debug(`Toggled members list: ${this.isMembersOpen}`);
	}

	subscribe(callback: () => void): () => void {
		return reaction(
			() => this.isMembersOpen,
			() => callback(),
			{fireImmediately: true},
		);
	}
}

export default new MemberListStore();
