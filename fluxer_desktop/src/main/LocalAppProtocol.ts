// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_APP_SCHEME} from '@electron/common/Constants';
import {createChildLogger} from '@electron/common/Logger';
import {getDesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import {recordGatewayOriginAnchor} from '@electron/main/GatewayOriginRegistry';
import {readStagedHarvestLocalStorageSync} from '@electron/main/LegacyOriginHarvestStore';
import type {LocalAppIndexContext} from '@electron/main/LocalAppFileRequestHandler';
import {DesktopLocalAppFiles} from '@electron/main/LocalAppFileResolver';
import {getDesktopLocalAppAuthorization} from '@electron/main/LocalAppProtocolAuthorization';
import {DesktopLocalAppProxyClient} from '@electron/main/LocalAppProxyClient';
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

export class LocalAppProtocolSchemeRegistrationOrderError extends Error {
	public constructor() {
		super('Local app protocol scheme privileges must be registered before Electron app ready');
		this.name = 'LocalAppProtocolSchemeRegistrationOrderError';
	}
}

export class LocalAppProtocolHandlerRegistrationOrderError extends Error {
	public constructor() {
		super('Local app protocol handler must be registered after Electron app ready');
		this.name = 'LocalAppProtocolHandlerRegistrationOrderError';
	}
}

export class LocalAppProtocolHandlerOwnershipError extends Error {
	public constructor() {
		super('Local app protocol already has a handler owned outside this runtime');
		this.name = 'LocalAppProtocolHandlerOwnershipError';
	}
}

export class LocalAppProtocolShutdownAdmissionError extends Error {
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
	private readonly files = new DesktopLocalAppFiles();
	private readonly requestHandler: DesktopLocalAppRequestHandler;
	private readonly stopObservingCommittedModuleFiles: () => void;

	public constructor() {
		this.stopObservingCommittedModuleFiles = observeCommittedModuleFiles(({root, files}) => {
			this.files.setModuleIndex(root === null ? null : {root, files});
		});
		this.requestHandler = new DesktopLocalAppRequestHandler({
			authorization: getDesktopLocalAppAuthorization(),
			files: this.files,
			indexContext: () => this.getIndexContext(),
			proxyClient: new DesktopLocalAppProxyClient({outboundHTTP: getDesktopOutboundHTTP()}),
			runtimePlans: this.runtimePlans,
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

	public register(): void {
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
		protocol.handle(DESKTOP_APP_SCHEME, (request) => this.trackOperation(() => this.requestHandler.handle(request)));
		this.ownsProtocolHandler = true;
		log.info('Registered the local app protocol handler');
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

	public getIndexContext(): LocalAppIndexContext {
		return {
			prebootTheme: readPrebootTheme(),
			cleartextInstanceOrigins: getDesktopOutboundHTTP().registeredCleartextOrigins(),
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
	try {
		const stored = readStagedHarvestLocalStorageSync();
		const theme = stored?.[THEME_STORAGE_KEY];
		return typeof theme === 'string' && theme.length > 0 ? theme : null;
	} catch {
		return null;
	}
}
