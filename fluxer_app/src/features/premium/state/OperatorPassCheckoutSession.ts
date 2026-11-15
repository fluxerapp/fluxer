// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable} from 'mobx';

interface CheckoutRun {
	startedPassCount: number;
	sessionIds: ReadonlyArray<string>;
	url: string;
	expiresAt: number;
}

class OperatorPassCheckoutSession {
	run: CheckoutRun | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get waiting(): boolean {
		return this.run !== null;
	}

	addSession(session: {id: string; url: string; expiresAt: number}, passCount: number): void {
		const previous = this.run;
		this.run = {
			startedPassCount: previous?.startedPassCount ?? passCount,
			sessionIds: previous?.sessionIds.includes(session.id)
				? previous.sessionIds
				: [...(previous?.sessionIds ?? []), session.id],
			url: session.url,
			expiresAt: Math.max(previous?.expiresAt ?? 0, session.expiresAt),
		};
	}

	adoptReturnedSession(sessionId: string, passCount: number): void {
		if (this.run?.sessionIds.includes(sessionId)) return;
		this.run = {
			startedPassCount: this.run?.startedPassCount ?? passCount,
			sessionIds: [...(this.run?.sessionIds ?? []), sessionId],
			url: this.run?.url ?? '',
			expiresAt: this.run?.expiresAt ?? 0,
		};
	}

	clear(): void {
		this.run = null;
	}
}

const operatorPassCheckoutSession = new OperatorPassCheckoutSession();

AccountScopedWork.registerCancellation(() => operatorPassCheckoutSession.clear());

export default operatorPassCheckoutSession;
