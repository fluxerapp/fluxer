// SPDX-License-Identifier: AGPL-3.0-or-later

import {sumMentionCounts} from '@app/features/gateway/transport/BackgroundGatewayAccounts';
import type {BackgroundGatewaySession} from '@app/features/gateway/transport/BackgroundGatewaySession';
import {makeAutoObservable, observable} from 'mobx';

class BackgroundGatewaySessionAlreadyRegisteredError extends Error {
	constructor(accountKey: string) {
		super(`A background gateway session is already registered for ${accountKey}`);
		this.name = 'BackgroundGatewaySessionAlreadyRegisteredError';
	}
}

class BackgroundGatewayConnectionRegistry {
	private readonly sessions = observable.map<string, BackgroundGatewaySession>({}, {deep: false});

	constructor() {
		makeAutoObservable<BackgroundGatewayConnectionRegistry, 'sessions'>(this, {sessions: false}, {autoBind: true});
	}

	get size(): number {
		return this.sessions.size;
	}

	get totalMentionCount(): number {
		return sumMentionCounts([...this.sessions.values()].map((session) => session.mentionCount));
	}

	get(accountKey: string): BackgroundGatewaySession | null {
		return this.sessions.get(accountKey) ?? null;
	}

	has(accountKey: string): boolean {
		return this.sessions.has(accountKey);
	}

	keys(): Array<string> {
		return [...this.sessions.keys()];
	}

	values(): Array<BackgroundGatewaySession> {
		return [...this.sessions.values()];
	}

	set(accountKey: string, session: BackgroundGatewaySession): void {
		if (this.sessions.has(accountKey)) {
			throw new BackgroundGatewaySessionAlreadyRegisteredError(accountKey);
		}
		this.sessions.set(accountKey, session);
	}

	delete(accountKey: string): boolean {
		return this.sessions.delete(accountKey);
	}

	getAccountMentionCount(accountKey: string): number {
		return this.sessions.get(accountKey)?.mentionCount ?? 0;
	}
}

export default new BackgroundGatewayConnectionRegistry();
