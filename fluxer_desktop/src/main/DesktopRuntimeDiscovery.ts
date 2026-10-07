// SPDX-License-Identifier: AGPL-3.0-or-later

import {getLaunchInstanceEndpointOverride} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {getDesktopAppStorage} from '@electron/main/DesktopAppStorage';
import {
	DesktopAddressRequirement,
	getDesktopOutboundHTTP,
	isDesktopHostResolutionFailure,
	requireDesktopHTTPOrigin,
} from '@electron/main/DesktopOutboundHTTP';
import {
	type LocalAppRuntimePlan,
	localAppRuntimePlanFromDiscovery,
	runtimePlanTrustedHTTPOrigins,
} from '@electron/main/LocalAppRuntimePlans';
import {getDesktopSelectedInstanceClient} from '@electron/main/SelectedInstanceFetch';
import {Headers as HttpHeader} from '@fluxer/constants/src/Headers';
import {HttpStatus, MimeType} from '@fluxer/constants/src/HttpConstants';
import {DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {
	buildInstanceDiscoveryCandidates,
	parseInstanceDiscoveryDocument,
	requireDiscoveryDeclaresItsOwnOrigin,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {officialClientApiEndpointForAlias} from '@fluxer/instance_bootstrap/src/OfficialInstance';

const log = createChildLogger('DesktopRuntimeDiscovery');

const DISCOVERY_TIMEOUT_MS = 15_000;
const LAST_SERVED_DISCOVERY_MARKER_PREFIX = 'runtime_discovery.v1:';

interface ResolveDesktopRuntimeConfigInput {
	readonly input: string;
	readonly signal: AbortSignal | null;
}

interface DiscoveryAttempt {
	readonly url: string;
	readonly reason: string;
}

interface LastServedDiscovery {
	readonly url: string;
	readonly document: unknown;
	readonly anchorRequirement?: DesktopAddressRequirement;
}

interface ResolvedRuntimePlan {
	readonly plan: LocalAppRuntimePlan;
	readonly served: LastServedDiscovery | null;
	readonly rememberedAnchorRequirement: DesktopAddressRequirement | null;
	readonly attempts: ReadonlyArray<DiscoveryAttempt>;
}

class InstanceAnsweredWithoutDiscoveryError extends Error {
	public constructor(reason: string) {
		super(reason);
		this.name = 'InstanceAnsweredWithoutDiscoveryError';
	}
}

class DesktopRuntimeEndpointInputError extends TypeError {
	public constructor(input: string) {
		super(`Desktop runtime endpoint input is not an absolute http(s) API endpoint: ${input}`);
		this.name = 'DesktopRuntimeEndpointInputError';
	}
}

class DesktopRuntimeDiscoveryFailedError extends Error {
	public readonly attempts: ReadonlyArray<DiscoveryAttempt>;

	public constructor(apiEndpoint: string, attempts: ReadonlyArray<DiscoveryAttempt>) {
		const described = attempts.map((attempt) => `${attempt.url} (${attempt.reason})`).join('; ');
		super(`No usable instance discovery document was served for ${apiEndpoint}: ${described}`);
		this.name = 'DesktopRuntimeDiscoveryFailedError';
		this.attempts = attempts;
	}
}

class DesktopRuntimeDiscoveryUnreachableError extends DesktopRuntimeDiscoveryFailedError {
	public constructor(apiEndpoint: string, attempts: ReadonlyArray<DiscoveryAttempt>) {
		super(apiEndpoint, attempts);
		this.name = DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME;
	}
}

function requireDesktopRuntimeAPIEndpoint(input: string): string {
	const normalized = normalizeInstanceEndpoint(input, InstanceEndpointKind.API);
	if (normalized == null || !normalized.includes('://')) {
		throw new DesktopRuntimeEndpointInputError(input);
	}
	return officialClientApiEndpointForAlias(normalized) ?? normalized;
}

export async function resolveDesktopRuntimePlan({
	input,
	signal,
}: ResolveDesktopRuntimeConfigInput): Promise<LocalAppRuntimePlan> {
	const apiEndpoint = requireDesktopRuntimeAPIEndpoint(input);
	const anchorOrigin = requireDesktopHTTPOrigin(new URL(apiEndpoint).origin);
	const resolved = await fetchRuntimePlan(apiEndpoint, anchorOrigin, signal);
	let anchorRequirement: DesktopAddressRequirement;
	try {
		anchorRequirement = await getDesktopOutboundHTTP().registerAnchoredOrigins({
			anchorOrigin,
			origins: runtimePlanTrustedHTTPOrigins(resolved.plan),
			unresolvedAnchorRequirement: resolved.rememberedAnchorRequirement,
		});
	} catch (error) {
		if (resolved.served === null && isDesktopHostResolutionFailure(error)) {
			throw new DesktopRuntimeDiscoveryUnreachableError(apiEndpoint, resolved.attempts);
		}
		throw error;
	}
	if (resolved.served !== null) {
		void rememberLastServedDiscovery(apiEndpoint, {...resolved.served, anchorRequirement});
	}
	return resolved.plan;
}

export function desktopRuntimeInitialInput(): string | null {
	return getLaunchInstanceEndpointOverride();
}

async function fetchRuntimePlan(
	apiEndpoint: string,
	anchorOrigin: string,
	signal: AbortSignal | null,
): Promise<ResolvedRuntimePlan> {
	const candidates = buildInstanceDiscoveryCandidates({input: apiEndpoint});
	const attempts: Array<DiscoveryAttempt> = [];
	let instanceUnreachable = candidates.length > 0;
	for (const url of candidates) {
		let document: unknown;
		try {
			document = await fetchDiscoveryDocument(url, anchorOrigin, signal);
		} catch (error) {
			instanceUnreachable &&= signal?.aborted !== true && !(error instanceof InstanceAnsweredWithoutDiscoveryError);
			attempts.push({url, reason: error instanceof Error ? error.message : String(error)});
			continue;
		}
		try {
			const plan = runtimePlanFromServedDiscovery(url, document);
			return {plan, served: {url, document}, rememberedAnchorRequirement: null, attempts};
		} catch (error) {
			instanceUnreachable = false;
			attempts.push({url, reason: error instanceof Error ? error.message : String(error)});
		}
	}
	if (!instanceUnreachable) {
		throw new DesktopRuntimeDiscoveryFailedError(apiEndpoint, attempts);
	}
	const lastServed = await readLastServedDiscovery(apiEndpoint);
	if (lastServed === null) {
		throw new DesktopRuntimeDiscoveryUnreachableError(apiEndpoint, attempts);
	}
	log.warn('Instance discovery is unreachable, using the last served discovery document', {apiEndpoint, attempts});
	return {
		plan: lastServed.plan,
		served: null,
		rememberedAnchorRequirement: lastServed.anchorRequirement,
		attempts,
	};
}

async function rememberLastServedDiscovery(apiEndpoint: string, served: LastServedDiscovery): Promise<void> {
	const storage = getDesktopAppStorage();
	if (storage === null) {
		return;
	}
	try {
		await storage.setMarker(`${LAST_SERVED_DISCOVERY_MARKER_PREFIX}${apiEndpoint}`, JSON.stringify(served));
	} catch (error) {
		log.warn('Failed to remember the served discovery document', {apiEndpoint, error});
	}
}

interface RememberedRuntimePlan {
	readonly plan: LocalAppRuntimePlan;
	readonly anchorRequirement: DesktopAddressRequirement | null;
}

function readAnchorRequirement(value: unknown): DesktopAddressRequirement | null {
	return value === DesktopAddressRequirement.ANY || value === DesktopAddressRequirement.PUBLIC ? value : null;
}

async function readLastServedDiscovery(apiEndpoint: string): Promise<RememberedRuntimePlan | null> {
	const storage = getDesktopAppStorage();
	if (storage === null) {
		return null;
	}
	try {
		const value = await storage.getMarker(`${LAST_SERVED_DISCOVERY_MARKER_PREFIX}${apiEndpoint}`);
		if (value === null) {
			return null;
		}
		const {url, document, anchorRequirement} = JSON.parse(value) as Partial<LastServedDiscovery>;
		if (typeof url !== 'string') {
			return null;
		}
		return {
			plan: runtimePlanFromServedDiscovery(url, document),
			anchorRequirement: readAnchorRequirement(anchorRequirement),
		};
	} catch (error) {
		log.warn('Failed to read the last served discovery document', {apiEndpoint, error});
		return null;
	}
}

export function runtimePlanFromServedDiscovery(url: string, document: unknown): LocalAppRuntimePlan {
	requireDiscoveryDeclaresItsOwnOrigin(url, parseInstanceDiscoveryDocument(document));
	return localAppRuntimePlanFromDiscovery(document);
}

async function fetchDiscoveryDocument(
	url: string,
	expectedOrigin: string,
	signal: AbortSignal | null,
): Promise<unknown> {
	const response = await getDesktopSelectedInstanceClient().fetch({
		expectedOrigin,
		headers: {[HttpHeader.ACCEPT]: MimeType.JSON},
		method: 'GET',
		signal,
		timeoutMs: DISCOVERY_TIMEOUT_MS,
		url,
	});
	if (!response.ok) {
		const reason = `responded HTTP ${response.status.toString()}`;
		throw response.status >= HttpStatus.INTERNAL_SERVER_ERROR
			? new Error(reason)
			: new InstanceAnsweredWithoutDiscoveryError(reason);
	}
	if (!isJSONContentType(response.headers['content-type'])) {
		throw new Error('responded without a JSON content type');
	}
	if (response.body == null) {
		throw new Error('responded without a body');
	}
	try {
		return JSON.parse(response.body.toString('utf8'));
	} catch {
		throw new InstanceAnsweredWithoutDiscoveryError('responded with a malformed JSON body');
	}
}

function isJSONContentType(value: string | ReadonlyArray<string> | undefined): boolean {
	const header = typeof value === 'string' ? value : value?.[0];
	if (header == null) {
		return false;
	}
	const mediaType = header.split(';')[0]?.trim().toLowerCase() ?? '';
	return mediaType === MimeType.JSON || mediaType.endsWith('+json');
}
