// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import GeoIP from '@app/features/app/state/GeoIP';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import Users from '@app/features/user/state/Users';
import type {
	OperatorPassCertificateLinkResponse,
	OperatorPassCheckoutRequest,
	OperatorPassCheckoutResponse,
	OperatorPassLabsJoinResponse,
	OperatorPassListingResponse,
	OperatorPassListingUpdateRequest,
	OperatorPassOfferAcceptRequest,
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
	OperatorPassPurchaseListResponse,
	OperatorPassPurchaseResponse,
	OperatorPassStateResponse,
	OperatorPassVisionaryTurnOnRequest,
	OperatorPassWithdrawRequest,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';

const logger = new Logger('OperatorPass');

export function getCountryCode(): string | undefined {
	return GeoIP.countryCode?.toUpperCase() ?? undefined;
}

function applyHolderToCurrentUser(state: OperatorPassStateResponse): void {
	const user = Users.currentUser;
	if (!user) return;
	const since = state.holder?.since ?? null;
	const current = user.operatorPassSince ?? null;
	if ((current?.getTime() ?? null) === (since ? new Date(since).getTime() : null)) return;
	Users.handleUserUpdate({...user.toJSON(), operator_pass_since: since});
	UserProfileCommands.clearCurrentUserProfiles();
}

export async function fetchState(): Promise<OperatorPassStateResponse> {
	const countryCode = getCountryCode();
	const response = await http.get<OperatorPassStateResponse>(Endpoints.OPERATOR_PASS_STATE, {
		query: countryCode ? {country_code: countryCode} : {},
	});
	applyHolderToCurrentUser(response.body);
	return response.body;
}

export async function createCheckout(request: OperatorPassCheckoutRequest): Promise<OperatorPassCheckoutResponse> {
	try {
		const response = await http.post<OperatorPassCheckoutResponse>(Endpoints.OPERATOR_PASS_CHECKOUT, {
			body: request,
		});
		logger.debug('Operator Pass checkout created', {quantity: request.quantity});
		return response.body;
	} catch (error) {
		logger.error('Operator Pass checkout failed', error);
		throw error;
	}
}

export async function fetchPurchases(returnedSessionId?: string): Promise<OperatorPassPurchaseListResponse> {
	const response = await http.get<OperatorPassPurchaseListResponse>(Endpoints.OPERATOR_PASS_PURCHASES, {
		query: returnedSessionId ? {session_id: returnedSessionId} : {},
	});
	return response.body;
}

export async function resetCertificateLink(passId: string): Promise<string> {
	const response = await http.post<OperatorPassCertificateLinkResponse>(
		Endpoints.OPERATOR_PASS_CERTIFICATE_LINK_RESET(passId),
	);
	logger.debug('Operator certificate link reset');
	return response.body.certificate_url;
}

export async function withdrawPurchase(
	purchaseId: string,
	request: OperatorPassWithdrawRequest,
): Promise<OperatorPassPurchaseResponse> {
	try {
		const response = await http.post<OperatorPassPurchaseResponse>(
			Endpoints.OPERATOR_PASS_PURCHASE_WITHDRAW(purchaseId),
			{
				body: request,
			},
		);
		logger.debug('Operator Pass purchase withdrawn', {quantity: response.body.quantity});
		UserProfileCommands.clearCurrentUserProfiles();
		return response.body;
	} catch (error) {
		logger.error('Operator Pass withdrawal failed', error);
		throw error;
	}
}

export async function fetchListing(): Promise<OperatorPassListingResponse> {
	const response = await http.get<OperatorPassListingResponse>(Endpoints.OPERATOR_PASS_LISTING);
	return response.body;
}

export async function updateListing(request: OperatorPassListingUpdateRequest): Promise<OperatorPassListingResponse> {
	try {
		const response = await http.put<OperatorPassListingResponse>(Endpoints.OPERATOR_PASS_LISTING, {body: request});
		logger.debug("Operators' wall listing updated", {optOut: request.wall_opt_out});
		return response.body;
	} catch (error) {
		logger.error("Operators' wall listing update failed", error);
		throw error;
	}
}

export async function fetchPasses(): Promise<OperatorPassPassesResponse> {
	const response = await http.get<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_PASSES);
	OperatorPassHoldings.setPasses(response.body);
	OperatorPassHoldings.markIncomingOffersSeen();
	return response.body;
}

export async function loadPassesForAccount(userId: string): Promise<void> {
	const response = await http.get<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_PASSES);
	if (Users.currentUser?.id !== userId) return;
	OperatorPassHoldings.setPasses(response.body);
	OperatorPassHoldings.markIncomingOffersSeen();
}

async function refreshPassesQuietly(): Promise<void> {
	try {
		await fetchPasses();
	} catch (error) {
		logger.warn('Operator Pass refresh failed', error);
	}
}

export async function createOffer(
	passId: string,
	recipientId: string,
	confirmLastPass: boolean,
): Promise<OperatorPassOfferResponse> {
	try {
		const response = await http.post<OperatorPassOfferResponse>(Endpoints.OPERATOR_PASS_PASS_OFFERS(passId), {
			body: {recipient_id: recipientId, confirm_last_pass: confirmLastPass},
		});
		logger.debug('Operator Pass offer created');
		await refreshPassesQuietly();
		return response.body;
	} catch (error) {
		logger.error('Operator Pass offer failed', error);
		throw error;
	}
}

export async function fetchOffer(offerId: string): Promise<OperatorPassOfferResponse> {
	const response = await http.get<OperatorPassOfferResponse>(Endpoints.OPERATOR_PASS_OFFER(offerId));
	return response.body;
}

export async function withdrawOffer(offerId: string): Promise<OperatorPassOfferResponse> {
	const response = await http.delete<OperatorPassOfferResponse>(Endpoints.OPERATOR_PASS_OFFER(offerId));
	logger.debug('Operator Pass offer withdrawn');
	await refreshPassesQuietly();
	return response.body;
}

export async function acceptOffer(
	offerId: string,
	request: OperatorPassOfferAcceptRequest,
): Promise<OperatorPassOfferResponse> {
	const response = await http.post<OperatorPassOfferResponse>(Endpoints.OPERATOR_PASS_OFFER_ACCEPT(offerId), {
		body: request,
	});
	logger.debug('Operator Pass offer accepted', {status: response.body.status});
	await refreshPassesQuietly();
	UserProfileCommands.clearCurrentUserProfiles();
	return response.body;
}

export async function declineOffer(offerId: string): Promise<OperatorPassOfferResponse> {
	const response = await http.post<OperatorPassOfferResponse>(Endpoints.OPERATOR_PASS_OFFER_DECLINE(offerId));
	logger.debug('Operator Pass offer declined');
	await refreshPassesQuietly();
	return response.body;
}

export async function withdrawPass(
	passId: string,
	request: OperatorPassWithdrawRequest,
): Promise<OperatorPassPassesResponse> {
	try {
		const response = await http.post<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_PASS_WITHDRAW(passId), {
			body: request,
		});
		logger.debug('Operator Pass withdrawn');
		OperatorPassHoldings.setPasses(response.body);
		UserProfileCommands.clearCurrentUserProfiles();
		return response.body;
	} catch (error) {
		logger.error('Operator Pass withdrawal failed', error);
		throw error;
	}
}

export async function updateLabsGreeting(greeting: boolean): Promise<OperatorPassPassesResponse> {
	try {
		const response = await http.put<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_LABS_GREETING, {
			body: {greeting},
		});
		logger.debug('#operators greeting updated', {greeting});
		OperatorPassHoldings.setPasses(response.body);
		return response.body;
	} catch (error) {
		logger.error('#operators greeting update failed', error);
		throw error;
	}
}

export async function updateLabsOperators(showOperators: boolean): Promise<OperatorPassPassesResponse> {
	try {
		const response = await http.put<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_LABS_OPERATORS, {
			body: {show_operators: showOperators},
		});
		logger.debug('#operators visibility updated', {showOperators});
		OperatorPassHoldings.setPasses(response.body);
		return response.body;
	} catch (error) {
		logger.error('#operators visibility update failed', error);
		throw error;
	}
}

export async function joinLabs(): Promise<OperatorPassLabsJoinResponse> {
	const response = await http.post<OperatorPassLabsJoinResponse>(Endpoints.OPERATOR_PASS_LABS_JOIN, {body: {}});
	logger.debug('Fluxer Labs join after Operator Pass', {status: response.body.status});
	return response.body;
}

export async function turnOnVisionaryPass(
	request: OperatorPassVisionaryTurnOnRequest,
): Promise<OperatorPassPassesResponse> {
	const response = await http.post<OperatorPassPassesResponse>(Endpoints.OPERATOR_PASS_VISIONARY_TURN_ON, {
		body: request,
	});
	logger.debug('Visionary Operator Pass turned on');
	OperatorPassHoldings.setPasses(response.body);
	UserProfileCommands.clearCurrentUserProfiles();
	return response.body;
}
