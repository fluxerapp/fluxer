// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ResolveWindowPidSpec {
	backend: 'gnome-shell-eval';
	token: string;
}

export declare function resolveWindowPid(spec: ResolveWindowPidSpec): Promise<number | null>;

export declare function resolveKwinWindowPid(token: string): Promise<number | null>;

export declare function resolveX11WindowPid(token: string): Promise<number | null>;

export type FileChooserFilterKind = 0 | 1;

export interface FileChooserFilterRule {
	kind: FileChooserFilterKind;
	pattern: string;
}

export interface FileChooserFilter {
	name: string;
	rules: ReadonlyArray<FileChooserFilterRule>;
}

export interface FileChooserOptions {
	parentWindow?: string;
	title?: string;
	acceptLabel?: string;
	modal?: boolean;
	multiple?: boolean;
	directory?: boolean;
	currentFolder?: string;
	currentName?: string;
	currentFile?: string;
	filters?: ReadonlyArray<FileChooserFilter>;
	currentFilter?: FileChooserFilter;
}

export interface FileChooserResult {
	cancelled: boolean;
	uris: Array<string>;
}

export declare function openFile(options: FileChooserOptions): Promise<FileChooserResult>;

export declare function saveFile(options: FileChooserOptions): Promise<FileChooserResult>;

export interface BackgroundOptions {
	reason?: string;
	autostart?: boolean;
	commandline?: ReadonlyArray<string>;
	dbusActivatable?: boolean;
}

export interface BackgroundResult {
	response: number;
	cancelled: boolean;
	background: boolean;
	autostart: boolean;
}

export declare function requestBackground(options: BackgroundOptions): Promise<BackgroundResult>;

export interface GlobalShortcutDefinition {
	id: string;
	description: string;
	preferredTrigger?: string | null;
}

export interface GlobalShortcutBinding {
	id: string;
	description: string | null;
	triggerDescription: string | null;
}

export type GlobalShortcutsPortalEvent =
	| {type: 'activated'; id: string}
	| {type: 'deactivated'; id: string}
	| {type: 'shortcuts-changed'; shortcuts: Array<GlobalShortcutBinding>}
	| {type: 'session-lost'; reason: 'closed' | 'portal-restarted' | 'bus-error'}
	| {type: 'portal-available'};

export interface GlobalShortcutsPortalOpenResult {
	version: number;
	appIdSource: 'sandbox' | 'registered' | 'unregistered';
	uniqueName: string;
	listed: Array<GlobalShortcutBinding>;
}

export type GlobalShortcutsBindOutcome =
	| {outcome: 'bound'; shortcuts: Array<GlobalShortcutBinding>}
	| {outcome: 'cancelled'}
	| {outcome: 'denied'}
	| {outcome: 'failed'; code: number};

export type GlobalShortcutsPortalDesktop = 'kde' | 'gnome' | 'hyprland' | 'other';

export interface GlobalShortcutsPortalOptions {
	portalAppId: string | null;
	sandboxed: boolean;
	sessionToken?: string;
	desktop?: GlobalShortcutsPortalDesktop;
}

export declare class GlobalShortcutsPortal {
	constructor(onEvent: (event: GlobalShortcutsPortalEvent) => void, options: GlobalShortcutsPortalOptions);

	open(): Promise<GlobalShortcutsPortalOpenResult>;

	bind(shortcuts: ReadonlyArray<GlobalShortcutDefinition>, parentWindow: string): Promise<GlobalShortcutsBindOutcome>;

	configure(parentWindow: string): Promise<void>;

	close(): void;
}

export type SessionStateMonitorEvent = {type: 'screen-locked'} | {type: 'screen-unlocked'};

export declare class SessionStateMonitor {
	constructor(onEvent: (event: SessionStateMonitorEvent) => void);

	close(): void;
}

export declare function readColorScheme(): 'no-preference' | 'prefer-dark' | 'prefer-light';

export declare function readContrast(): 'no-preference' | 'high';

export declare function readAccentColor(): {r: number; g: number; b: number} | null;

export interface SettingsChangeEvent {
	namespace: string;
	key: string;
	uint32?: number;
	accent?: {r: number; g: number; b: number};
}

export declare class Settings {
	constructor(onChange: (event: SettingsChangeEvent) => void);

	close(): void;
}

export declare const loadError: Error | null;
