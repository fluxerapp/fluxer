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

import {makeAutoObservable} from 'mobx';
import {type BetaCode, BetaCodeRecord} from '~/records/BetaCodeRecord';

type FetchStatus = 'idle' | 'pending' | 'success' | 'error';

class BetaCodeStore {
	betaCodes: Array<BetaCodeRecord> = [];
	fetchStatus: FetchStatus = 'idle';
	isCreateError = false;
	isDeleteError = false;
	allowance = 3;
	nextResetAt: Date | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	fetchPending(): void {
		this.fetchStatus = 'pending';
	}

	fetchSuccess(betaCodes: ReadonlyArray<BetaCode>, allowance: number, nextResetAt: string | null): void {
		this.betaCodes = betaCodes.map((betaCode) => new BetaCodeRecord(betaCode));
		this.fetchStatus = 'success';
		this.allowance = allowance;
		this.nextResetAt = nextResetAt ? new Date(nextResetAt) : null;
	}

	fetchError(): void {
		this.fetchStatus = 'error';
	}

	createPending(): void {
		this.isCreateError = false;
	}

	createSuccess(betaCode: BetaCodeRecord): void {
		this.betaCodes = [...this.betaCodes, betaCode];
		this.allowance = Math.max(0, this.allowance - 1);
	}

	createError(): void {
		this.isCreateError = true;
	}

	deletePending(): void {
		this.isDeleteError = false;
	}

	deleteSuccess(code: string): void {
		const removed = this.betaCodes.find((betaCode) => betaCode.code === code);
		this.betaCodes = this.betaCodes.filter((betaCode) => betaCode.code !== code);
		if (removed && !removed.redeemer) {
			this.allowance = this.allowance + 1;
		}
	}

	deleteError(): void {
		this.isDeleteError = true;
	}
}

export default new BetaCodeStore();
