// SPDX-License-Identifier: AGPL-3.0-or-later

import {randomBytes} from 'node:crypto';
import {DESKTOP_APP_ORIGIN} from '@electron/common/Constants';
import {isLocalAppRendererDocumentURL, isLocalAppURL} from '@electron/main/LocalAppURL';
import {webContents as electronWebContents, webFrameMain as electronWebFrameMain} from 'electron';

export const DESKTOP_PROTOCOL_AUTHORIZATION_HEADER = 'X-Fluxer-Desktop-Protocol-Authorization';

const DESKTOP_PROTOCOL_AUTHORIZATION_HEADER_LOWER = DESKTOP_PROTOCOL_AUTHORIZATION_HEADER.toLowerCase();
const AUTHORIZATION_SECRET_BYTES = 32;
const MAIN_FRAME_RESOURCE_TYPE = 'mainFrame';
const BLANK_FRAME_URLS: ReadonlySet<string> = new Set(['', 'about:blank']);

interface LocalAppRequestSnapshot {
	readonly url: string;
	readonly resourceType: string;
	readonly frame: Electron.WebFrameMain | null;
	readonly webContents: Electron.WebContents | null;
	readonly webContentsId: number | null;
}

class InvalidLocalAppRequestDetailsError extends TypeError {
	constructor() {
		super('Electron request details have invalid or inconsistent fields');
		this.name = 'InvalidLocalAppRequestDetailsError';
	}
}

class InvalidLocalAppWebContentsError extends TypeError {
	constructor() {
		super('Local app authorization requires a live Electron WebContents reference');
		this.name = 'InvalidLocalAppWebContentsError';
	}
}

export class DesktopLocalAppAuthorization {
	private readonly authorizationSecret = randomBytes(AUTHORIZATION_SECRET_BYTES).toString('hex');
	private readonly authorizedWebContents = new Map<number, Electron.WebContents>();

	authorize(webContents: Electron.WebContents): void {
		if (!isWebContentsReference(webContents) || webContents.isDestroyed()) {
			throw new InvalidLocalAppWebContentsError();
		}
		const webContentsId = webContents.id;
		const registered = this.authorizedWebContents.get(webContentsId);
		if (registered != null) {
			if (registered !== webContents) {
				throw new InvalidLocalAppWebContentsError();
			}
			return;
		}
		this.authorizedWebContents.set(webContentsId, webContents);
		webContents.once('destroyed', () => {
			if (this.authorizedWebContents.get(webContentsId) === webContents) {
				this.authorizedWebContents.delete(webContentsId);
			}
		});
	}

	hasValidRequestAuthorization(request: Request): boolean {
		return request.headers.get(DESKTOP_PROTOCOL_AUTHORIZATION_HEADER) === this.authorizationSecret;
	}

	applyRequestHeaders(details: unknown, headers: Record<string, string>): Record<string, string> {
		const next = withoutAuthorizationHeader(headers);
		if (!this.canAuthorizeRequest(details)) {
			return next;
		}
		next[DESKTOP_PROTOCOL_AUTHORIZATION_HEADER] = this.authorizationSecret;
		return next;
	}

	private canAuthorizeRequest(details: unknown): boolean {
		if (!isLocalAppURL(readRequestURL(details))) {
			return false;
		}
		let snapshot: LocalAppRequestSnapshot;
		try {
			snapshot = readRequestSnapshot(details);
		} catch {
			return false;
		}
		const owner = this.resolveAuthorizedWebContents(snapshot);
		if (owner == null) {
			return false;
		}
		const frame = snapshot.frame;
		if (frame == null || !isFrameOwnedByWebContents(frame, owner)) {
			return false;
		}
		if (isLocalAppMainFrameBootstrap(snapshot, frame, owner)) {
			return true;
		}
		return isTrustedLocalAppFrame(frame);
	}

	private resolveAuthorizedWebContents(snapshot: LocalAppRequestSnapshot): Electron.WebContents | null {
		const webContentsId = snapshot.webContentsId;
		if (webContentsId == null) {
			return null;
		}
		const registered = this.authorizedWebContents.get(webContentsId);
		if (registered == null) {
			return null;
		}
		try {
			if (registered.isDestroyed() || registered.id !== webContentsId) {
				return null;
			}
		} catch {
			return null;
		}
		if (snapshot.webContents != null && snapshot.webContents !== registered) {
			return null;
		}
		return registered;
	}
}

let authorization: DesktopLocalAppAuthorization | null = null;

export function getDesktopLocalAppAuthorization(): DesktopLocalAppAuthorization {
	authorization ??= new DesktopLocalAppAuthorization();
	return authorization;
}

function isLocalAppMainFrameBootstrap(
	snapshot: LocalAppRequestSnapshot,
	frame: Electron.WebFrameMain,
	owner: Electron.WebContents,
): boolean {
	if (snapshot.resourceType !== MAIN_FRAME_RESOURCE_TYPE) {
		return false;
	}
	if (!isLocalAppRendererDocumentURL(snapshot.url)) {
		return false;
	}
	try {
		if (frame.parent != null || frame.top !== frame) {
			return false;
		}
		return owner.mainFrame === frame;
	} catch {
		return false;
	}
}

function isTrustedLocalAppFrame(frame: Electron.WebFrameMain): boolean {
	try {
		const frameURL: unknown = frame.url;
		if (typeof frameURL !== 'string') {
			return false;
		}
		if (isLocalAppRendererDocumentURL(frameURL)) {
			return true;
		}
		if (!BLANK_FRAME_URLS.has(frameURL)) {
			return false;
		}
		return frame.origin === DESKTOP_APP_ORIGIN;
	} catch {
		return false;
	}
}

function isFrameOwnedByWebContents(frame: Electron.WebFrameMain, owner: Electron.WebContents): boolean {
	try {
		if (frame.isDestroyed() || frame.detached) {
			return false;
		}
		return electronWebContents.fromFrame(frame) === owner;
	} catch {
		return false;
	}
}

function withoutAuthorizationHeader(headers: Record<string, string>): Record<string, string> {
	const next: Record<string, string> = Object.create(null) as Record<string, string>;
	for (const [name, value] of Object.entries(headers)) {
		if (name.toLowerCase() !== DESKTOP_PROTOCOL_AUTHORIZATION_HEADER_LOWER) {
			next[name] = value;
		}
	}
	return next;
}

function readObjectRecord(value: unknown): Readonly<Record<string, unknown>> | null {
	if (typeof value !== 'object' || value === null) {
		return null;
	}
	return value as Readonly<Record<string, unknown>>;
}

function readRequestURL(details: unknown): string | null {
	const record = readObjectRecord(details);
	if (record == null || typeof record.url !== 'string') {
		return null;
	}
	return record.url;
}

function isWebContentsReference(value: unknown): value is Electron.WebContents {
	const record = readObjectRecord(value);
	if (record == null) {
		return false;
	}
	let id: unknown;
	try {
		id = record.id;
	} catch {
		return false;
	}
	if (typeof id !== 'number' || !Number.isSafeInteger(id) || id <= 0) {
		return false;
	}
	let resolved: unknown;
	try {
		resolved = electronWebContents.fromId(id);
	} catch {
		return false;
	}
	return resolved === value;
}

function isWebFrameMainReference(value: unknown): value is Electron.WebFrameMain {
	const record = readObjectRecord(value);
	if (record == null) {
		return false;
	}
	let processId: unknown;
	let routingId: unknown;
	try {
		processId = record.processId;
		routingId = record.routingId;
	} catch {
		return false;
	}
	if (typeof processId !== 'number' || !Number.isSafeInteger(processId) || processId < 0) {
		return false;
	}
	if (typeof routingId !== 'number' || !Number.isSafeInteger(routingId) || routingId < 0) {
		return false;
	}
	let resolved: unknown;
	try {
		resolved = electronWebFrameMain.fromId(processId, routingId);
	} catch {
		return false;
	}
	return resolved === value;
}

function readRequestSnapshot(details: unknown): LocalAppRequestSnapshot {
	const record = readObjectRecord(details);
	if (record == null) {
		throw new InvalidLocalAppRequestDetailsError();
	}
	const url = record.url;
	const resourceType = record.resourceType;
	if (typeof url !== 'string' || typeof resourceType !== 'string') {
		throw new InvalidLocalAppRequestDetailsError();
	}
	const frame = readOptionalWebFrameMain(record.frame);
	let webContents = readOptionalWebContents(record.webContents);
	let webContentsId = readOptionalWebContentsId(record.webContentsId);
	if (webContents != null) {
		const ownerId = readWebContentsId(webContents);
		if (webContentsId != null && webContentsId !== ownerId) {
			throw new InvalidLocalAppRequestDetailsError();
		}
		webContentsId = ownerId;
	}
	if (frame != null) {
		let frameWebContents: unknown;
		try {
			frameWebContents = electronWebContents.fromFrame(frame);
		} catch {
			throw new InvalidLocalAppRequestDetailsError();
		}
		if (!isWebContentsReference(frameWebContents)) {
			throw new InvalidLocalAppRequestDetailsError();
		}
		const frameWebContentsId = readWebContentsId(frameWebContents);
		if (webContents != null && webContents !== frameWebContents) {
			throw new InvalidLocalAppRequestDetailsError();
		}
		if (webContentsId != null && webContentsId !== frameWebContentsId) {
			throw new InvalidLocalAppRequestDetailsError();
		}
		webContents = frameWebContents;
		webContentsId = frameWebContentsId;
	}
	return Object.freeze({url, resourceType, frame, webContents, webContentsId});
}

function readOptionalWebContents(value: unknown): Electron.WebContents | null {
	if (value == null) {
		return null;
	}
	if (!isWebContentsReference(value)) {
		throw new InvalidLocalAppRequestDetailsError();
	}
	return value;
}

function readOptionalWebFrameMain(value: unknown): Electron.WebFrameMain | null {
	if (value == null) {
		return null;
	}
	if (!isWebFrameMainReference(value)) {
		throw new InvalidLocalAppRequestDetailsError();
	}
	return value;
}

function readOptionalWebContentsId(value: unknown): number | null {
	if (value == null) {
		return null;
	}
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value <= 0) {
		throw new InvalidLocalAppRequestDetailsError();
	}
	return value;
}

function readWebContentsId(webContents: Electron.WebContents): number {
	let value: unknown;
	try {
		value = webContents.id;
	} catch {
		throw new InvalidLocalAppRequestDetailsError();
	}
	const webContentsId = readOptionalWebContentsId(value);
	if (webContentsId == null) {
		throw new InvalidLocalAppRequestDetailsError();
	}
	return webContentsId;
}
