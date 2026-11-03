// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPublicKey} from 'node:crypto';

const ACTIVITY_STREAMS_CONTEXT = 'https://www.w3.org/ns/activitystreams';
const SECURITY_CONTEXT = 'https://w3id.org/security/v1';

export const ACTIVITY_PUB_CONTENT_TYPE = 'application/activity+json';
export const WEBFINGER_CONTENT_TYPE = 'application/jrd+json';

function activityPubActorUrl(apiPublic: string): string {
	return `${apiPublic.replace(/\/+$/u, '')}/activitypub/actor`;
}

export function activityPubKeyId(apiPublic: string): string {
	return `${activityPubActorUrl(apiPublic)}#main-key`;
}

export function activityPubPublicKeyPem(pkcs1Base64: string): string {
	return createPublicKey({key: Buffer.from(pkcs1Base64, 'base64'), format: 'der', type: 'pkcs1'})
		.export({type: 'spki', format: 'pem'})
		.toString();
}

function actorAcct(actorUrl: string): string {
	const url = new URL(actorUrl);
	return `${url.hostname}@${url.host}`;
}

export function buildActivityPubActor(apiPublic: string, publicKeyPem: string) {
	const actorUrl = activityPubActorUrl(apiPublic);
	return {
		'@context': [ACTIVITY_STREAMS_CONTEXT, SECURITY_CONTEXT],
		id: actorUrl,
		type: 'Application',
		preferredUsername: new URL(actorUrl).hostname,
		inbox: `${actorUrl}/inbox`,
		outbox: `${actorUrl}/outbox`,
		manuallyApprovesFollowers: true,
		publicKey: {
			id: activityPubKeyId(apiPublic),
			owner: actorUrl,
			publicKeyPem,
		},
	};
}

export function buildActivityPubOutbox(apiPublic: string) {
	return {
		'@context': ACTIVITY_STREAMS_CONTEXT,
		id: `${activityPubActorUrl(apiPublic)}/outbox`,
		type: 'OrderedCollection',
		totalItems: 0,
		orderedItems: [],
	};
}

export function buildWebFinger(apiPublic: string, resource: string | undefined) {
	const actorUrl = activityPubActorUrl(apiPublic);
	const acct = actorAcct(actorUrl);
	const requested = resource?.trim() ?? '';
	if (requested.toLowerCase() !== `acct:${acct}`.toLowerCase() && requested !== actorUrl) {
		return null;
	}
	return {
		subject: `acct:${acct}`,
		links: [{rel: 'self', type: ACTIVITY_PUB_CONTENT_TYPE, href: actorUrl}],
	};
}
