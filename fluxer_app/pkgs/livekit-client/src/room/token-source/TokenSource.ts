// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {RoomAgentDispatch, RoomConfiguration, TokenSourceRequest, TokenSourceResponse} from '@livekit/protocol';
import {
	TokenSourceConfigurable,
	type TokenSourceFetchOptions,
	TokenSourceFixed,
	type TokenSourceResponseObject,
} from './types.ts';
import {areTokenSourceFetchOptionsEqual, decodeTokenPayload, isResponseTokenValid} from './utils.ts';

abstract class TokenSourceCached extends TokenSourceConfigurable {
	private cachedFetchOptions: TokenSourceFetchOptions | null = null;

	private cachedResponse: TokenSourceResponse | null = null;

	private fetchMutex = new Mutex();
	private isSameAsCachedFetchOptions(options: TokenSourceFetchOptions) {
		if (!this.cachedFetchOptions) {
			return false;
		}
		return areTokenSourceFetchOptionsEqual(options, this.cachedFetchOptions);
	}

	private shouldReturnCachedValueFromFetch(fetchOptions: TokenSourceFetchOptions) {
		if (!this.cachedResponse) {
			return false;
		}
		if (!isResponseTokenValid(this.cachedResponse)) {
			return false;
		}
		if (!this.isSameAsCachedFetchOptions(fetchOptions)) {
			return false;
		}
		return true;
	}

	getCachedResponseJwtPayload() {
		if (!this.cachedResponse) {
			return null;
		}
		return decodeTokenPayload(this.cachedResponse.participantToken);
	}

	async fetch(options: TokenSourceFetchOptions, force?: boolean): Promise<TokenSourceResponseObject> {
		const unlock = await this.fetchMutex.lock();
		try {
			if (force) {
				this.cachedResponse = null;
			}
			if (this.shouldReturnCachedValueFromFetch(options)) {
				return this.cachedResponse!.toJson() as TokenSourceResponseObject;
			}
			this.cachedFetchOptions = options;

			const tokenResponse = await this.update(options);
			this.cachedResponse = tokenResponse;
			return tokenResponse.toJson() as TokenSourceResponseObject;
		} finally {
			unlock();
		}
	}

	protected abstract update(options: TokenSourceFetchOptions): Promise<TokenSourceResponse>;
}

type LiteralOrFn = TokenSourceResponseObject | (() => TokenSourceResponseObject | Promise<TokenSourceResponseObject>);
class TokenSourceLiteral extends TokenSourceFixed {
	private literalOrFn: LiteralOrFn;

	constructor(literalOrFn: LiteralOrFn) {
		super();
		this.literalOrFn = literalOrFn;
	}

	async fetch(): Promise<TokenSourceResponseObject> {
		if (typeof this.literalOrFn === 'function') {
			return this.literalOrFn();
		} else {
			return this.literalOrFn;
		}
	}
}

type CustomFn = (options: TokenSourceFetchOptions) => TokenSourceResponseObject | Promise<TokenSourceResponseObject>;
class TokenSourceCustom extends TokenSourceCached {
	private customFn: CustomFn;

	constructor(customFn: CustomFn) {
		super();
		this.customFn = customFn;
	}

	protected async update(options: TokenSourceFetchOptions) {
		const result = await this.customFn(options);

		return TokenSourceResponse.fromJson(result, {
			ignoreUnknownFields: true,
		});
	}
}

export type EndpointOptions = Omit<RequestInit, 'body'>;

class TokenSourceEndpoint extends TokenSourceCached {
	private url: string;

	private endpointOptions: EndpointOptions;

	constructor(url: string, options: EndpointOptions = {}) {
		super();
		this.url = url;
		this.endpointOptions = options;
	}

	private createRequestFromOptions(options: TokenSourceFetchOptions) {
		const request = new TokenSourceRequest();

		for (const key of Object.keys(options) as Array<keyof TokenSourceFetchOptions>) {
			switch (key) {
				case 'roomName':
				case 'participantName':
				case 'participantIdentity':
				case 'participantMetadata':
					request[key] = options[key];
					break;

				case 'participantAttributes':
					request.participantAttributes = options.participantAttributes ?? {};
					break;

				case 'agentName':
					request.roomConfig = request.roomConfig ?? new RoomConfiguration();
					if (request.roomConfig.agents.length === 0) {
						request.roomConfig.agents.push(new RoomAgentDispatch());
					}
					request.roomConfig.agents[0].agentName = options.agentName!;
					break;

				case 'agentMetadata':
					request.roomConfig = request.roomConfig ?? new RoomConfiguration();
					if (request.roomConfig.agents.length === 0) {
						request.roomConfig.agents.push(new RoomAgentDispatch());
					}
					request.roomConfig.agents[0].metadata = options.agentMetadata!;
					break;

				case 'deployment':
					request.roomConfig = request.roomConfig ?? new RoomConfiguration();
					if (request.roomConfig.agents.length === 0) {
						request.roomConfig.agents.push(new RoomAgentDispatch());
					}
					request.roomConfig.agents[0].deployment = options.deployment!;
					break;

				default: {
					const exhaustiveCheckedKey: never = key;
					throw new Error(`Options key ${exhaustiveCheckedKey} not being included in forming request!`);
				}
			}
		}

		return request;
	}

	protected async update(options: TokenSourceFetchOptions) {
		const request = this.createRequestFromOptions(options);

		const response = await fetch(this.url, {
			...this.endpointOptions,
			method: this.endpointOptions.method ?? 'POST',
			headers: {
				'Content-Type': 'application/json',
				...this.endpointOptions.headers,
			},
			body: request.toJsonString({
				useProtoFieldName: true,
			}),
		});

		if (!response.ok) {
			throw new Error(
				`Error generating token from endpoint ${this.url}: received ${response.status} / ${await response.text()}`,
			);
		}

		const body = await response.json();
		return TokenSourceResponse.fromJson(body, {
			ignoreUnknownFields: true,
		});
	}
}

export type DevelopmentTokenServerOptions = {
	baseUrl?: string;
};
class TokenSourceDevelopmentTokenServer extends TokenSourceEndpoint {
	constructor(tokenServerId: string, options: DevelopmentTokenServerOptions) {
		const {baseUrl = 'https://cloud-api.livekit.io', ...rest} = options;

		super(`${baseUrl}/api/v2/sandbox/connection-details`, {
			...rest,
			headers: {
				'X-Sandbox-ID': tokenServerId,
			},
		});
	}
}

export type SandboxTokenServerOptions = DevelopmentTokenServerOptions;

class TokenSourceSandboxTokenServer extends TokenSourceDevelopmentTokenServer {}

export {
	areTokenSourceFetchOptionsEqual,
	decodeTokenPayload,
	type TokenSourceCustom,
	type TokenSourceDevelopmentTokenServer,
	type TokenSourceEndpoint,
	type TokenSourceLiteral,
	type TokenSourceSandboxTokenServer,
};

export const TokenSource = {
	literal(literalOrFn: LiteralOrFn) {
		return new TokenSourceLiteral(literalOrFn);
	},

	custom(customFn: CustomFn) {
		return new TokenSourceCustom(customFn);
	},

	endpoint(url: string, options: EndpointOptions = {}) {
		return new TokenSourceEndpoint(url, options);
	},

	sandboxTokenServer(sandboxId: string, options: SandboxTokenServerOptions = {}) {
		return new TokenSourceSandboxTokenServer(sandboxId, options);
	},

	developmentTokenServer(tokenServerId: string, options: DevelopmentTokenServerOptions = {}) {
		return new TokenSourceDevelopmentTokenServer(tokenServerId, options);
	},
};
