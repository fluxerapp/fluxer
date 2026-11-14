// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import type {
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {makeAutoObservable} from 'mobx';

class OperatorPassHoldings {
	passes: OperatorPassPassesResponse | null = null;
	private announcedOfferIds = new Set<string>();
	private sessionStartedAt = Date.now();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get count(): number | null {
		return this.passes?.count ?? null;
	}

	findOffer(offerId: string): OperatorPassOfferResponse | null {
		const passes = this.passes;
		if (!passes) return null;
		return (
			passes.incoming_offers.find((offer) => offer.id === offerId) ??
			passes.outgoing_offers.find((offer) => offer.id === offerId) ??
			null
		);
	}

	setPasses(passes: OperatorPassPassesResponse): void {
		this.passes = passes;
	}

	markIncomingOffersSeen(): void {
		for (const offer of this.passes?.incoming_offers ?? []) {
			this.announcedOfferIds.add(offer.id);
		}
	}

	takeNewIncomingOffers(): Array<OperatorPassOfferResponse> {
		const fresh = (this.passes?.incoming_offers ?? []).filter(
			(offer) =>
				offer.status === 'pending' &&
				!this.announcedOfferIds.has(offer.id) &&
				Date.parse(offer.created_at) >= this.sessionStartedAt,
		);
		this.markIncomingOffersSeen();
		return fresh;
	}

	clear(): void {
		this.passes = null;
		this.sessionStartedAt = Date.now();
	}
}

const operatorPassHoldings = new OperatorPassHoldings();

AccountScopedWork.registerCancellation(() => operatorPassHoldings.clear());

export default operatorPassHoldings;
