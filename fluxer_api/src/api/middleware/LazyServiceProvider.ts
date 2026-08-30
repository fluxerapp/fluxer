// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Context} from 'hono';
import type {HonoEnv} from '../types/HonoEnv';

type RequestVariables = HonoEnv['Variables'];

type EagerlySetVariable = 'apiContext' | 'sudoModeValid';

type ExternallySetVariable =
	| 'adminApiKey'
	| 'adminApiKeyAcls'
	| 'adminUserAcls'
	| 'adminUserId'
	| 'auditLogReason'
	| 'authSession'
	| 'authToken'
	| 'authTokenType'
	| 'authUserId'
	| 'authViaCookie'
	| 'channelUpdateType'
	| 'oauthBearerAllowed'
	| 'oauthBearerApplicationId'
	| 'oauthBearerScopes'
	| 'oauthBearerToken'
	| 'oauthBearerUserId'
	| 'requestCache'
	| 'requestId'
	| 'requestLocale'
	| 'responseSchema'
	| 'sudoModeToken'
	| 'user';

type ConfigurationDependentService = 'ageVerificationService' | 'donationService';

export type RequestScopedServices = Omit<
	RequestVariables,
	EagerlySetVariable | ExternallySetVariable | ConfigurationDependentService
> & {
	readonly [Key in ConfigurationDependentService]: RequestVariables[Key] | undefined;
};

export type LazyServiceProvider = {
	readonly [Key in keyof RequestVariables]?: RequestVariables[Key];
};

export function installLazyServices(ctx: Context<HonoEnv>, provider: LazyServiceProvider): void {
	const readVariable = ctx.get;
	const writeVariable = ctx.set;
	ctx.get = <Key extends keyof RequestVariables>(key: Key): RequestVariables[Key] => {
		const existing = readVariable(key);
		if (existing !== undefined) {
			return existing;
		}
		const resolved = provider[key];
		if (resolved === undefined) {
			return existing;
		}
		writeVariable(key, resolved);
		return resolved;
	};
}
