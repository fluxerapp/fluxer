// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_SCHEME} from '@electron/common/Constants';
import {getLaunchProductName, getPrebootTheme} from '@electron/common/DesktopConfig';
import {createChildLogger} from '@electron/common/Logger';
import {resolveDesktopLandingUrl} from '@electron/main/DesktopLastRoute';
import {getDesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import {restoreRememberedRuntimePlan} from '@electron/main/DesktopRuntimeDiscovery';
import {recordGatewayOriginAnchor} from '@electron/main/GatewayOriginRegistry';
import {readStagedHarvestLocalStorageSync} from '@electron/main/LegacyOriginHarvestStore';
import {LOCAL_APP_REWRITTEN_FILE_MAX_BYTES, type LocalAppIndexContext} from '@electron/main/LocalAppFileRequestHandler';
import {DesktopLocalAppFiles, LocalAppFileResolution} from '@electron/main/LocalAppFileResolver';
import {readDeclaredBootStateFormat} from '@electron/main/LocalAppIndexHTML';
import {getDesktopLocalAppAuthorization} from '@electron/main/LocalAppProtocolAuthorization';
import {DesktopLocalAppProxyClient, LocalAppProxyTraffic} from '@electron/main/LocalAppProxyClient';
import {DesktopLocalAppRequestHandler} from '@electron/main/LocalAppRequestHandler';
import {
	DesktopLocalAppRuntimePlans,
	type LocalAppRuntimePlan,
	runtimePlanTrustedHTTPOrigins,
} from '@electron/main/LocalAppRuntimePlans';
import {observeCommittedModuleFiles} from '@electron/main/ModuleBootHandoff';
import {app, protocol} from 'electron';

const log = createChildLogger('LocalAppProtocol');

const THEME_STORAGE_KEY = 'theme';

interface LocalAppStartup {
	readonly rendererReadiness: Promise<boolean | null>;
	readonly bootStatePrewarmed: () => Promise<void>;
	readonly admitBootFrame: (declaredFormat: string | null) => boolean;
	readonly readBootSession: () => Promise<boolean | null>;
}

class LocalAppProtocolSchemeRegistrationOrderError extends Error {
	public constructor() {
		super('Local app protocol scheme privileges must be registered before Electron app ready');
		this.name = 'LocalAppProtocolSchemeRegistrationOrderError';
	}
}

class LocalAppProtocolHandlerRegistrationOrderError extends Error {
	public constructor() {
		super('Local app protocol handler must be registered after Electron app ready');
		this.name = 'LocalAppProtocolHandlerRegistrationOrderError';
	}
}

class LocalAppProtocolHandlerOwnershipError extends Error {
	public constructor() {
		super('Local app protocol already has a handler owned outside this runtime');
		this.name = 'LocalAppProtocolHandlerOwnershipError';
	}
}

class LocalAppProtocolShutdownAdmissionError extends Error {
	public constructor() {
		super('Local app protocol is shutting down');
		this.name = 'LocalAppProtocolShutdownAdmissionError';
	}
}

export class DesktopLocalAppProtocol {
	private ownsProtocolHandler = false;
	private acceptingRequests = true;
	private cleanupOperation: Promise<void> | null = null;
	private readonly shutdownController = new AbortController();
	private readonly activeOperations = new Set<Promise<unknown>>();
	private readonly runtimePlans = new DesktopLocalAppRuntimePlans();
	private readonly planRestorations = new Map<string, Promise<LocalAppRuntimePlan | null>>();
	private readonly files = new DesktopLocalAppFiles();
	private readonly requestHandler: DesktopLocalAppRequestHandler;
	private readonly stopObservingCommittedModuleFiles: () => void;
	private startup: LocalAppStartup | null = null;
	private initialBootSessionUsed = false;

	public constructor() {
		this.stopObservingCommittedModuleFiles = observeCommittedModuleFiles(({root, files}) => {
			this.files.setModuleIndex(root === null ? null : {root, files});
		});
		this.requestHandler = new DesktopLocalAppRequestHandler({
			authorization: getDesktopLocalAppAuthorization(),
			files: this.files,
			indexContext: () => this.getIndexContext(),
			apiProxyClient: new DesktopLocalAppProxyClient({
				outboundHTTP: getDesktopOutboundHTTP(),
				traffic: LocalAppProxyTraffic.API,
			}),
			resourceProxyClient: new DesktopLocalAppProxyClient({
				outboundHTTP: getDesktopOutboundHTTP(),
				traffic: LocalAppProxyTraffic.REMOTE_RESOURCE,
			}),
			runtimePlans: {planForRoute: (runtimeKey) => this.planForRoute(runtimeKey)},
			shutdownSignal: this.shutdownController.signal,
		});
	}

	public registerSchemes(): void {
		if (schemePrivilegesRegistered()) {
			return;
		}
		if (app.isReady()) {
			throw new LocalAppProtocolSchemeRegistrationOrderError();
		}
		protocol.registerSchemesAsPrivileged([
			{
				scheme: DESKTOP_APP_SCHEME,
				privileges: {
					standard: true,
					secure: true,
					supportFetchAPI: true,
					corsEnabled: true,
					stream: true,
					codeCache: true,
				},
			},
		]);
		markSchemePrivilegesRegistered();
	}

	public register(startup: LocalAppStartup): void {
		this.requireAdmission();
		if (this.ownsProtocolHandler) {
			return;
		}
		if (!app.isReady()) {
			throw new LocalAppProtocolHandlerRegistrationOrderError();
		}
		if (protocol.isProtocolHandled(DESKTOP_APP_SCHEME)) {
			throw new LocalAppProtocolHandlerOwnershipError();
		}
		this.startup = startup;
		protocol.handle(DESKTOP_APP_SCHEME, (request) =>
			this.trackOperation(async () => {
				await this.waitForRenderer(request.signal);
				this.requireAdmission();
				return this.requestHandler.handle(request);
			}),
		);
		this.ownsProtocolHandler = true;
		log.info('Registered the local app protocol handler');
	}

	public whenReadyToServe(): Promise<boolean | null> {
		if (this.startup === null) throw new Error('The renderer readiness gate is not registered');
		return this.startup.rendererReadiness;
	}

	private waitForRenderer(requestSignal: AbortSignal): Promise<void> {
		const signal = AbortSignal.any([requestSignal, this.shutdownController.signal]);
		return new Promise((resolve, reject) => {
			const abort = (): void => reject(signal.reason);
			if (signal.aborted) {
				abort();
				return;
			}
			signal.addEventListener('abort', abort, {once: true});
			void this.whenReadyToServe().then(
				() => {
					signal.removeEventListener('abort', abort);
					if (signal.aborted) reject(signal.reason);
					else resolve();
				},
				(error: unknown) => {
					signal.removeEventListener('abort', abort);
					reject(error);
				},
			);
		});
	}

	public getShutdownSignal(): AbortSignal {
		return this.shutdownController.signal;
	}

	public getActivePlan(): LocalAppRuntimePlan | null {
		return this.runtimePlans.getActivePlan();
	}

	public findPlanForRoute(runtimeKey: string): LocalAppRuntimePlan | null {
		return this.runtimePlans.findPlanForRoute(runtimeKey);
	}

	private planForRoute(runtimeKey: string): Promise<LocalAppRuntimePlan | null> {
		const known = this.runtimePlans.findPlanForRoute(runtimeKey);
		if (known != null) {
			return Promise.resolve(known);
		}
		let restoration = this.planRestorations.get(runtimeKey);
		if (restoration == null) {
			restoration = this.restoreRuntimePlan(runtimeKey).finally(() => {
				this.planRestorations.delete(runtimeKey);
			});
			this.planRestorations.set(runtimeKey, restoration);
		}
		return restoration;
	}

	private async restoreRuntimePlan(runtimeKey: string): Promise<LocalAppRuntimePlan | null> {
		let plan: LocalAppRuntimePlan | null;
		try {
			plan = await restoreRememberedRuntimePlan(runtimeKey);
		} catch (error) {
			log.warn('Failed to restore a remembered local app runtime plan', {instanceKey: runtimeKey, error});
			return null;
		}
		if (plan == null || !this.acceptingRequests) {
			return null;
		}
		const known = this.runtimePlans.findPlanForRoute(runtimeKey);
		if (known != null) {
			return known;
		}
		this.cacheRuntimePlan(plan);
		return plan;
	}

	public cacheRuntimePlan(plan: LocalAppRuntimePlan): void {
		this.requireAdmission();
		this.requireRuntimeOriginsRegistered(plan);
		this.runtimePlans.cache(plan);
		recordGatewayOriginAnchor(plan.endpoints.apiEndpoint, plan.endpoints.gatewayEndpoint);
	}

	public activateRuntimePlan(plan: LocalAppRuntimePlan): void {
		this.requireAdmission();
		this.requireRuntimeOriginsRegistered(plan);
		this.runtimePlans.activate(plan);
		recordGatewayOriginAnchor(plan.endpoints.apiEndpoint, plan.endpoints.gatewayEndpoint);
		log.info('Activated a local app runtime plan', {instanceKey: plan.instanceKey});
	}

	public deactivateRuntimePlan(): void {
		this.requireAdmission();
		this.runtimePlans.deactivate();
		log.info('Deactivated the local app runtime plan');
	}

	public async prepareBootFrame(
		readiness: Promise<unknown>,
		prepareFrame: (declaredFormat: string | null) => Promise<void>,
	): Promise<void> {
		await readiness;
		this.requireAdmission();
		const resolved = await this.files.resolve(resolveDesktopLandingUrl(app.getPath('userData')));
		if (resolved.type !== LocalAppFileResolution.FILE || !this.files.isIndexFile(resolved.filePath)) return;
		const file = await this.files.readForResponse(
			resolved.filePath,
			LOCAL_APP_REWRITTEN_FILE_MAX_BYTES,
			this.shutdownController.signal,
		);
		try {
			await prepareFrame(readDeclaredBootStateFormat(file.contents.toString('utf8')));
		} finally {
			file.release();
		}
	}

	private getIndexContext(): LocalAppIndexContext {
		const startup = this.startup;
		if (startup === null) throw new Error('The renderer readiness gate is not registered');
		return {
			prebootTheme: readPrebootTheme(),
			productName: getLaunchProductName(),
			bootStateExpectedFor: startup.admitBootFrame,
			bootStatePrewarmed: startup.bootStatePrewarmed,
			bootSession: () => {
				if (this.initialBootSessionUsed) return startup.readBootSession();
				this.initialBootSessionUsed = true;
				return startup.rendererReadiness;
			},
			instanceOrigins: getDesktopOutboundHTTP().registeredInstanceOrigins(),
		};
	}

	public cleanup(): Promise<void> {
		this.cleanupOperation ??= this.runCleanup();
		return this.cleanupOperation;
	}

	private requireRuntimeOriginsRegistered(plan: LocalAppRuntimePlan): void {
		getDesktopOutboundHTTP().requireRegisteredOrigins(runtimePlanTrustedHTTPOrigins(plan));
	}

	private requireAdmission(): void {
		if (!this.acceptingRequests) {
			throw new LocalAppProtocolShutdownAdmissionError();
		}
	}

	private trackOperation(operation: () => Promise<Response>): Promise<Response> {
		const tracked = Promise.resolve()
			.then(operation)
			.finally(() => {
				this.activeOperations.delete(tracked);
			});
		this.activeOperations.add(tracked);
		return tracked;
	}

	private async runCleanup(): Promise<void> {
		this.acceptingRequests = false;
		this.stopObservingCommittedModuleFiles();
		this.shutdownController.abort(new LocalAppProtocolShutdownAdmissionError());
		if (this.ownsProtocolHandler) {
			this.ownsProtocolHandler = false;
			try {
				protocol.unhandle(DESKTOP_APP_SCHEME);
			} catch (error) {
				log.warn('Failed to unhandle the local app protocol', {error});
			}
		}
		await Promise.allSettled([...this.activeOperations]);
	}
}

let sharedLocalAppProtocol: DesktopLocalAppProtocol | null = null;

export function getDesktopLocalAppProtocol(): DesktopLocalAppProtocol {
	sharedLocalAppProtocol ??= new DesktopLocalAppProtocol();
	return sharedLocalAppProtocol;
}

export async function cleanupDesktopLocalAppProtocol(): Promise<void> {
	const protocolOwner = sharedLocalAppProtocol;
	sharedLocalAppProtocol = null;
	await protocolOwner?.cleanup();
}

const SCHEME_PRIVILEGES_LATCH = Symbol.for('fluxer.desktop.LocalAppProtocol.schemePrivileges');

interface SchemePrivilegesLatchHost {
	[SCHEME_PRIVILEGES_LATCH]?: boolean;
}

function schemePrivilegesRegistered(): boolean {
	return (globalThis as SchemePrivilegesLatchHost)[SCHEME_PRIVILEGES_LATCH] === true;
}

function markSchemePrivilegesRegistered(): void {
	(globalThis as SchemePrivilegesLatchHost)[SCHEME_PRIVILEGES_LATCH] = true;
}

function readPrebootTheme(): string | null {
	return getPrebootTheme() ?? readHarvestedPrebootTheme();
}

function readHarvestedPrebootTheme(): string | null {
	try {
		const stored = readStagedHarvestLocalStorageSync();
		const theme = stored?.[THEME_STORAGE_KEY];
		return typeof theme === 'string' && theme.length > 0 ? theme : null;
	} catch {
		return null;
	}
}
