// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {BLUESKY_PROVIDER_NAME, TRAKT_PROVIDER_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import UserConnection from '@app/features/connection/state/UserConnection';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as FailureInspect from '@app/features/platform/utils/ResponseInspection';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {ConnectionType} from '@fluxer/constants/src/ConnectionConstants';
import type {
	ConnectionListResponse,
	ConnectionResponse,
	ConnectionVerificationResponse,
	CreateConnectionRequest,
	ReorderConnectionsRequest,
	UpdateConnectionRequest,
	VerifyAndCreateConnectionRequest,
} from '@fluxer/schema/src/domains/connection/ConnectionSchemas';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const FAILED_TO_INITIATE_CONNECTION_DESCRIPTOR = msg({
	message: "Couldn't start the connection",
	comment: 'Toast error shown when creating a new account connection fails.',
});
const FAILED_TO_START_AUTHORISATION_DESCRIPTOR = msg({
	message: "Couldn't start {blueskyProviderName} authorisation",
	comment:
		'Toast error shown when starting the OAuth flow for a connection provider fails. Preserve {blueskyProviderName}; it is inserted by code and must appear verbatim in the translation.',
});
const CONNECTION_VERIFIED_DESCRIPTOR = msg({
	message: 'Connection verified',
	comment: 'Success toast after an account connection is verified.',
});
const FAILED_TO_VERIFY_CONNECTION_DESCRIPTOR = msg({
	message: "Couldn't verify the connection",
	comment: 'Toast error shown when verifying an account connection fails.',
});
const CONNECTION_UPDATED_DESCRIPTOR = msg({
	message: 'Connection updated',
	comment: 'Short label in the connection connection commands.',
});
const CONNECTION_REMOVED_DESCRIPTOR = msg({
	message: 'Connection removed',
	comment: 'Short label in the connection connection commands.',
});
const CONNECTIONS_REORDERED_DESCRIPTOR = msg({
	message: 'Connections reordered',
	comment: 'Short label in the connection connection commands.',
});
const logger = new Logger('Connections');

interface BlueskyAuthorizeResponse {
	authorize_url: string;
}

interface TraktAuthorizeResponse {
	authorize_url: string;
}

function connectionCreateRequest(type: ConnectionType, identifier: string): CreateConnectionRequest {
	return {
		type,
		identifier,
	};
}

function verifyAndCreateRequest(initiationToken: string, visibilityFlags?: number): VerifyAndCreateConnectionRequest {
	return {
		initiation_token: initiationToken,
		visibility_flags: visibilityFlags,
	};
}

function reorderConnectionsRequest(connectionIds: Array<string>): ReorderConnectionsRequest {
	return {
		connection_ids: connectionIds,
	};
}

function successToast(i18n: I18n, message: MessageDescriptor): void {
	ToastCommands.createToast({
		type: 'success',
		children: i18n._(message),
	});
}

function showErrorModal(i18n: I18n, error: unknown, fallbackMessage: MessageDescriptor): void {
	const errorMessage = FailureInspect.failureMessage(error);
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message: () => errorMessage ?? i18n._(fallbackMessage),
		dataFlx: 'connection.commands.connection-error-modal',
	});
}

export async function fetchConnections(): Promise<void> {
	try {
		const response = await http.get<ConnectionListResponse>(Endpoints.CONNECTIONS);
		UserConnection.setConnections(response.body);
		logger.debug('Successfully fetched connections');
	} catch (error) {
		logger.error('Failed to fetch connections:', error);
		throw error;
	}
}

export async function initiateConnection(
	i18n: I18n,
	type: ConnectionType,
	identifier: string,
): Promise<ConnectionVerificationResponse> {
	try {
		const response = await http.post<ConnectionVerificationResponse>(Endpoints.CONNECTIONS, {
			body: connectionCreateRequest(type, identifier),
		});
		logger.debug(`Successfully initiated connection: ${type}/${identifier}`);
		return response.body;
	} catch (error) {
		logger.error(`Failed to initiate connection ${type}/${identifier}:`, error);
		showErrorModal(i18n, error, FAILED_TO_INITIATE_CONNECTION_DESCRIPTOR);
		throw error;
	}
}

export async function authorizeBlueskyConnection(i18n: I18n, handle: string): Promise<void> {
	try {
		const response = await http.post<BlueskyAuthorizeResponse>(Endpoints.BLUESKY_AUTHORIZE, {body: {handle}});
		window.open(response.body.authorize_url, '_blank');
	} catch (error) {
		logger.error(`Failed to start Bluesky OAuth flow for ${handle}:`, error);
		showErrorModal(i18n, error, {
			...FAILED_TO_START_AUTHORISATION_DESCRIPTOR,
			values: {blueskyProviderName: BLUESKY_PROVIDER_NAME},
		});
		throw error;
	}
}

export async function authorizeTraktConnection(i18n: I18n): Promise<void> {
	try {
		const response = await http.post<TraktAuthorizeResponse>(Endpoints.TRAKT_AUTHORIZE, {body: {}});
		window.open(response.body.authorize_url, '_blank');
	} catch (error) {
		logger.error('Failed to start Trakt OAuth flow:', error);
		showErrorModal(i18n, error, {
			...FAILED_TO_START_AUTHORISATION_DESCRIPTOR,
			values: {blueskyProviderName: TRAKT_PROVIDER_NAME},
		});
		throw error;
	}
}

export async function verifyAndCreateConnection(
	i18n: I18n,
	initiationToken: string,
	visibilityFlags?: number,
): Promise<ConnectionResponse> {
	try {
		const response = await http.post<ConnectionResponse>(Endpoints.CONNECTIONS_VERIFY_AND_CREATE, {
			body: verifyAndCreateRequest(initiationToken, visibilityFlags),
		});
		UserConnection.addConnection(response.body);
		UserProfileCommands.clearCurrentUserProfiles();
		successToast(i18n, CONNECTION_VERIFIED_DESCRIPTOR);
		logger.debug('Successfully verified and created connection');
		return response.body;
	} catch (error) {
		logger.error('Failed to verify and create connection:', error);
		showErrorModal(i18n, error, FAILED_TO_VERIFY_CONNECTION_DESCRIPTOR);
		throw error;
	}
}

export async function updateConnection(
	i18n: I18n,
	type: string,
	connectionId: string,
	patch: UpdateConnectionRequest,
): Promise<void> {
	try {
		await http.patch(Endpoints.CONNECTION(type, connectionId), {body: patch});
		UserConnection.updateConnection(connectionId, patch);
		UserProfileCommands.clearCurrentUserProfiles();
		successToast(i18n, CONNECTION_UPDATED_DESCRIPTOR);
		logger.debug(`Successfully updated connection: ${type}/${connectionId}`);
	} catch (error) {
		logger.error(`Failed to update connection ${type}/${connectionId}:`, error);
		throw error;
	}
}

export async function deleteConnection(i18n: I18n, type: string, connectionId: string): Promise<void> {
	try {
		await http.delete(Endpoints.CONNECTION(type, connectionId));
		UserConnection.removeConnection(connectionId);
		UserProfileCommands.clearCurrentUserProfiles();
		successToast(i18n, CONNECTION_REMOVED_DESCRIPTOR);
		logger.debug(`Successfully deleted connection: ${type}/${connectionId}`);
	} catch (error) {
		logger.error(`Failed to delete connection ${type}/${connectionId}:`, error);
		throw error;
	}
}

export async function verifyConnection(i18n: I18n, type: string, connectionId: string): Promise<void> {
	try {
		const response = await http.post<ConnectionResponse>(Endpoints.CONNECTION_VERIFY(type, connectionId), {
			body: {},
		});
		UserConnection.updateConnection(connectionId, response.body);
		UserProfileCommands.clearCurrentUserProfiles();
		successToast(i18n, CONNECTION_VERIFIED_DESCRIPTOR);
		logger.debug(`Successfully verified connection: ${type}/${connectionId}`);
	} catch (error) {
		logger.error(`Failed to verify connection ${type}/${connectionId}:`, error);
		throw error;
	}
}

export async function reorderConnections(i18n: I18n, connectionIds: Array<string>): Promise<void> {
	try {
		await http.patch(Endpoints.CONNECTIONS_REORDER, {body: reorderConnectionsRequest(connectionIds)});
		await fetchConnections();
		UserProfileCommands.clearCurrentUserProfiles();
		successToast(i18n, CONNECTIONS_REORDERED_DESCRIPTOR);
		logger.debug('Successfully reordered connections');
	} catch (error) {
		logger.error('Failed to reorder connections:', error);
		throw error;
	}
}
