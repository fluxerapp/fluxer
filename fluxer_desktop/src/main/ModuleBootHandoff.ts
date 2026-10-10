// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import type {DesktopModuleEnsureResult} from '@fluxer/desktop_ipc/src/ModuleContract';
import {app, type BrowserWindow} from 'electron';

const logger = createChildLogger('ModuleBootHandoff');

type MainWindowListener = (window: BrowserWindow | null) => void;

type MainWindowReadyListener = () => void;

type OnDemandModuleInstaller = (moduleName: string) => Promise<DesktopModuleEnsureResult>;

type SecondInstanceSink = (argv: Array<string>) => void;

type OpenUrlSink = (url: string) => void;

type MainWindowFactory = () => BrowserWindow;

interface CommittedModuleFileIndex {
	readonly root: string | null;
	readonly files: ReadonlyMap<string, string>;
}

type CommittedModuleFilesListener = (index: CommittedModuleFileIndex) => void;

interface ModuleBootHandoffState {
	committedModuleFileIndex: CommittedModuleFileIndex;
	mainWindowSignalled: boolean;
	mainWindowReadySignalled: boolean;
	mainWindow: BrowserWindow | null;
	onDemandModuleInstaller: OnDemandModuleInstaller | null;
	mainWindowFactory: MainWindowFactory | null;
	secondInstanceForwarding: boolean;
	secondInstanceSink: SecondInstanceSink | null;
	readonly bufferedSecondInstances: Array<Array<string>>;
	openUrlForwarding: boolean;
	openUrlSink: OpenUrlSink | null;
	readonly bufferedOpenUrls: Array<string>;
	readonly committedModuleFilesListeners: Set<CommittedModuleFilesListener>;
	readonly mainWindowListeners: Set<MainWindowListener>;
	readonly mainWindowReadyListeners: Set<MainWindowReadyListener>;
}

const handoffState: ModuleBootHandoffState = {
	committedModuleFileIndex: {root: null, files: new Map()},
	mainWindowSignalled: false,
	mainWindowReadySignalled: false,
	mainWindow: null,
	onDemandModuleInstaller: null,
	mainWindowFactory: null,
	secondInstanceForwarding: false,
	secondInstanceSink: null,
	bufferedSecondInstances: [],
	openUrlForwarding: false,
	openUrlSink: null,
	bufferedOpenUrls: [],
	committedModuleFilesListeners: new Set(),
	mainWindowListeners: new Set(),
	mainWindowReadyListeners: new Set(),
};

function deliverSecondInstance(sink: SecondInstanceSink, argv: Array<string>): void {
	try {
		sink(argv);
	} catch (error) {
		logger.error('A forwarded second instance launch threw', error);
	}
}

export function armSecondInstanceForwarding(): void {
	if (handoffState.secondInstanceForwarding) return;
	handoffState.secondInstanceForwarding = true;
	app.on('second-instance', (_event, argv) => {
		const sink = handoffState.secondInstanceSink;
		if (sink == null) {
			handoffState.bufferedSecondInstances.push(argv);
			return;
		}
		deliverSecondInstance(sink, argv);
	});
}

export function setSecondInstanceSink(sink: SecondInstanceSink): void {
	handoffState.secondInstanceSink = sink;
	const buffered = handoffState.bufferedSecondInstances.splice(0);
	for (const argv of buffered) {
		deliverSecondInstance(sink, argv);
	}
}

function deliverOpenUrl(sink: OpenUrlSink, url: string): void {
	try {
		sink(url);
	} catch (error) {
		logger.error('A forwarded open-url threw', error);
	}
}

function forwardOpenUrl(url: string): void {
	const sink = handoffState.openUrlSink;
	if (sink == null) {
		handoffState.bufferedOpenUrls.push(url);
		return;
	}
	deliverOpenUrl(sink, url);
}

export function armOpenUrlForwarding(): void {
	if (handoffState.openUrlForwarding) return;
	handoffState.openUrlForwarding = true;
	app.on('open-url', (event, url) => {
		event.preventDefault();
		forwardOpenUrl(url);
	});
	app.once('ready', (_event, launchInfo) => {
		const fallbackDeepLink = (launchInfo as {userInfo?: {fallbackDeepLink?: unknown}} | undefined)?.userInfo
			?.fallbackDeepLink;
		if (typeof fallbackDeepLink === 'string') {
			forwardOpenUrl(fallbackDeepLink);
		}
	});
}

export function setOpenUrlSink(sink: OpenUrlSink): void {
	handoffState.openUrlSink = sink;
	const buffered = handoffState.bufferedOpenUrls.splice(0);
	for (const url of buffered) {
		deliverOpenUrl(sink, url);
	}
}

export function setCommittedModuleFiles(root: string, files: ReadonlyMap<string, string>): void {
	handoffState.committedModuleFileIndex = {
		root: files.size === 0 ? null : root,
		files: new Map(files),
	};
	for (const listener of Array.from(handoffState.committedModuleFilesListeners)) {
		try {
			listener(handoffState.committedModuleFileIndex);
		} catch (error) {
			logger.error('A committed module file listener threw', error);
		}
	}
}

export function observeCommittedModuleFiles(listener: CommittedModuleFilesListener): () => void {
	handoffState.committedModuleFilesListeners.add(listener);
	listener(handoffState.committedModuleFileIndex);
	return () => {
		handoffState.committedModuleFilesListeners.delete(listener);
	};
}

export function setOnDemandModuleInstaller(installer: OnDemandModuleInstaller | null): void {
	handoffState.onDemandModuleInstaller = installer;
}

export function getOnDemandModuleInstaller(): OnDemandModuleInstaller | null {
	return handoffState.onDemandModuleInstaller;
}

export function setMainWindowFactory(factory: MainWindowFactory): void {
	handoffState.mainWindowFactory = factory;
}

export function getMainWindowFactory(): MainWindowFactory | null {
	return handoffState.mainWindowFactory;
}

export function signalMainWindowCreated(window: BrowserWindow | null): void {
	if (!handoffState.mainWindowSignalled) {
		handoffState.mainWindowSignalled = true;
		handoffState.mainWindow = window;
		const listeners = Array.from(handoffState.mainWindowListeners);
		handoffState.mainWindowListeners.clear();
		for (const listener of listeners) {
			try {
				listener(window);
			} catch (error) {
				logger.error('A main window handoff listener threw', error);
			}
		}
	}
}

export function onMainWindowCreated(listener: MainWindowListener): void {
	if (handoffState.mainWindowSignalled) {
		listener(handoffState.mainWindow);
		return;
	}
	handoffState.mainWindowListeners.add(listener);
}

export function signalMainWindowReady(): void {
	if (handoffState.mainWindowReadySignalled) {
		return;
	}
	handoffState.mainWindowReadySignalled = true;
	const listeners = Array.from(handoffState.mainWindowReadyListeners);
	handoffState.mainWindowReadyListeners.clear();
	for (const listener of listeners) {
		try {
			listener();
		} catch (error) {
			logger.error('A main window ready listener threw', error);
		}
	}
}

export function onMainWindowReady(listener: MainWindowReadyListener): void {
	if (handoffState.mainWindowReadySignalled) {
		listener();
		return;
	}
	handoffState.mainWindowReadyListeners.add(listener);
}

const rendererLaunchConfirmationListeners = new Set<() => void>();

export function signalRendererLaunchConfirmed(): void {
	for (const listener of Array.from(rendererLaunchConfirmationListeners)) {
		try {
			listener();
		} catch (error) {
			logger.error('A renderer launch confirmation listener threw', error);
		}
	}
}

export function observeRendererLaunchConfirmed(listener: () => void): () => void {
	rendererLaunchConfirmationListeners.add(listener);
	return () => {
		rendererLaunchConfirmationListeners.delete(listener);
	};
}
