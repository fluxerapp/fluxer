// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import os from 'node:os';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import type {
	AppMetricsSnapshot,
	CpuInfo,
	DesktopInfo,
	GpuDeviceInfo,
	GpuInfo,
	ProcessMetrics,
} from '@electron/common/Types';
import {isPortableMode} from '@electron/common/UserDataPath';
import {getFlatpakAppId, isFlatpakRuntime} from '@electron/main/LinuxSandbox';
import {app} from 'electron';

const requireModule = createRequire(import.meta.url);

type MacSysctlModule = {
	sysctlByNameInt: ((name: string) => Promise<number | null>) | null;
	sysctlByNameString: ((name: string) => Promise<string | null>) | null;
	loadError: Error | null;
};
type NativeGpuInfoSource = 'metal' | 'dxgi' | 'linux-sysfs';
type NativePlatformInfoModule = {
	getGpuInfo:
		| (() => {
				devices: ReadonlyArray<GpuDeviceInfo>;
				source: NativeGpuInfoSource;
		  })
		| null;
	loadError: Error | null;
};

let macSysctlCache: MacSysctlModule | null | undefined;
let nativePlatformInfoCache: NativePlatformInfoModule | null | undefined;

const CHROMIUM_RUNTIME_SWITCHES = [
	'autoplay-policy',
	'disable_accelerated_h264_decode',
	'disable_accelerated_h264_encode',
	'disable_accelerated_hevc_decode',
	'disable_d3d11',
	'disable_d3d11_video_decoder',
	'disable-background-timer-throttling',
	'disable_decode_swap_chain',
	'disable_dxgi_zero_copy_video',
	'disable_dynamic_video_encode_framerate_update',
	'disable_media_foundation_clear_playback',
	'disable_media_foundation_frame_size_change',
	'disable_metal',
	'disable-renderer-backgrounding',
	'disable_nv12_dxgi_video',
	'enable-libopenh264',
	'enable-h264-mf',
	'enable-h264-mf-zero-copy',
	'force_high_performance_gpu',
	'force_low_power_gpu',
	'openh264-library-path',
];

interface DesktopInfoOptions {
	nativeProbes?: boolean;
}

function splitChromiumFeatureSwitch(value: string): Array<string> {
	if (!value) return [];
	return value
		.split(',')
		.map((part) => part.trim())
		.filter(Boolean)
		.sort((a, b) => a.localeCompare(b));
}

function getChromiumRuntimeInfo(): DesktopInfo['chromiumRuntime'] {
	return {
		enableFeatures: splitChromiumFeatureSwitch(app.commandLine.getSwitchValue('enable-features')),
		disableFeatures: splitChromiumFeatureSwitch(app.commandLine.getSwitchValue('disable-features')),
		switches: CHROMIUM_RUNTIME_SWITCHES.filter((name) => app.commandLine.hasSwitch(name)),
	};
}

function loadMacSysctl(): MacSysctlModule | null {
	if (macSysctlCache !== undefined) return macSysctlCache;
	if (process.platform !== 'darwin') {
		macSysctlCache = null;
		return null;
	}
	try {
		macSysctlCache = requireModule('@fluxer/mac-sysctl') as MacSysctlModule;
	} catch (error) {
		throw new Error(
			`@fluxer/mac-sysctl failed to load on macOS; native diagnostics cannot continue. Original error: ${
				error instanceof Error ? error.message : String(error)
			}`,
		);
	}
	return macSysctlCache;
}

function loadNativePlatformInfo(): NativePlatformInfoModule | null {
	if (nativePlatformInfoCache !== undefined) return nativePlatformInfoCache;
	try {
		nativePlatformInfoCache = requireModule('@fluxer/platform-info') as NativePlatformInfoModule;
	} catch {
		nativePlatformInfoCache = null;
	}
	return nativePlatformInfoCache;
}

async function detectRosettaMode(): Promise<boolean> {
	const mod = loadMacSysctl();
	if (!mod?.sysctlByNameInt) return false;
	try {
		const translated = await mod.sysctlByNameInt('sysctl.proc_translated');
		return translated === 1;
	} catch {
		return false;
	}
}

async function detectHardwareArch(): Promise<string> {
	const mod = loadMacSysctl();
	if (!mod?.sysctlByNameInt) return os.arch();
	try {
		const optionalArm64 = await mod.sysctlByNameInt('hw.optional.arm64');
		if (optionalArm64 === 1) return 'arm64';
	} catch {}
	return os.arch();
}

export async function getDesktopInfo(options: DesktopInfoOptions = {}): Promise<DesktopInfo> {
	const nativeProbes = options.nativeProbes ?? true;
	const [hardwareArch, runningUnderRosetta] = nativeProbes
		? await Promise.all([detectHardwareArch(), detectRosettaMode()])
		: [os.arch(), false];
	return {
		version: app.getVersion(),
		channel: BUILD_CHANNEL,
		arch: process.arch,
		hardwareArch,
		runningUnderRosetta,
		os: process.platform,
		osVersion: os.release(),
		systemVersion: process.getSystemVersion(),
		electronVersion: process.versions.electron ?? 'unknown',
		chromeVersion: process.versions.chrome ?? 'unknown',
		nodeVersion: process.versions.node ?? 'unknown',
		waylandSession:
			process.platform === 'linux' &&
			(Boolean(process.env.WAYLAND_DISPLAY) || process.env.XDG_SESSION_TYPE === 'wayland'),
		portable: isPortableMode(),
		flatpak: isFlatpakRuntime(),
		flatpakAppId: getFlatpakAppId(),
		chromiumRuntime: getChromiumRuntimeInfo(),
	};
}

function vendorName(vendorId: number): string | undefined {
	switch (vendorId) {
		case 0x1002:
		case 0x1022:
			return 'AMD';
		case 0x106b:
			return 'Apple';
		case 0x10de:
			return 'NVIDIA';
		case 0x1414:
			return 'Microsoft';
		case 0x5143:
			return 'Qualcomm';
		case 0x8086:
			return 'Intel';
		default:
			return undefined;
	}
}

function normalizeElectronGpuDevice(device: {
	active?: boolean;
	vendorId?: number;
	deviceId?: number;
	deviceString?: string;
	driverVendor?: string;
	driverVersion?: string;
}): GpuDeviceInfo {
	const vendorId = typeof device.vendorId === 'number' ? device.vendorId : 0;
	return {
		active: Boolean(device.active),
		vendorId,
		deviceId: typeof device.deviceId === 'number' ? device.deviceId : 0,
		vendorName: vendorName(vendorId),
		deviceString: device.deviceString || undefined,
		driverVendor: device.driverVendor || undefined,
		driverVersion: device.driverVersion || undefined,
		source: 'electron',
	};
}

function nativeDeviceKey(device: GpuDeviceInfo): string | null {
	if (device.registryId) return `registry:${device.registryId}`;
	if (device.adapterLuid) return `luid:${device.adapterLuid}`;
	if (device.pciPath) return `pci:${device.pciPath}`;
	if (device.vendorId || device.deviceId) return `pciid:${device.vendorId}:${device.deviceId}`;
	return null;
}

function sameDevice(a: GpuDeviceInfo, b: GpuDeviceInfo): boolean {
	const ak = nativeDeviceKey(a);
	const bk = nativeDeviceKey(b);
	if (ak && bk && ak === bk) return true;
	return a.vendorId !== 0 && a.vendorId === b.vendorId && a.deviceId !== 0 && a.deviceId === b.deviceId;
}

function mergeGpuDevice(nativeDevice: GpuDeviceInfo, electronDevice: GpuDeviceInfo | undefined): GpuDeviceInfo {
	if (!electronDevice) return nativeDevice;
	return {
		...electronDevice,
		...nativeDevice,
		active: nativeDevice.active || electronDevice.active,
		vendorName: nativeDevice.vendorName ?? electronDevice.vendorName,
		deviceString: nativeDevice.deviceString ?? electronDevice.deviceString,
		driverVendor: nativeDevice.driverVendor ?? electronDevice.driverVendor,
		driverVersion: nativeDevice.driverVersion ?? electronDevice.driverVersion,
	};
}

function mergeGpuDevices(
	nativeDevices: ReadonlyArray<GpuDeviceInfo>,
	electronDevices: ReadonlyArray<GpuDeviceInfo>,
): Array<GpuDeviceInfo> {
	if (nativeDevices.length === 0) return [...electronDevices];
	const usedElectronIndexes = new Set<number>();
	const merged = nativeDevices.map((nativeDevice) => {
		const electronIndex = electronDevices.findIndex(
			(candidate, index) => !usedElectronIndexes.has(index) && sameDevice(nativeDevice, candidate),
		);
		if (electronIndex === -1) return nativeDevice;
		usedElectronIndexes.add(electronIndex);
		return mergeGpuDevice(nativeDevice, electronDevices[electronIndex]);
	});
	for (const [index, electronDevice] of electronDevices.entries()) {
		if (!usedElectronIndexes.has(index)) merged.push(electronDevice);
	}
	return merged;
}

function getNativeGpuInfo(): {devices: ReadonlyArray<GpuDeviceInfo>; source: NativeGpuInfoSource} | null {
	const mod = loadNativePlatformInfo();
	if (!mod?.getGpuInfo) return null;
	try {
		return mod.getGpuInfo();
	} catch {
		return null;
	}
}

export async function getGpuInfo(): Promise<GpuInfo> {
	const native = getNativeGpuInfo();
	try {
		const raw = (await app.getGPUInfo('complete')) as {
			gpuDevice?: ReadonlyArray<{
				active?: boolean;
				vendorId?: number;
				deviceId?: number;
				deviceString?: string;
				driverVendor?: string;
				driverVersion?: string;
			}>;
			auxAttributes?: {
				glRenderer?: string;
				glVendor?: string;
			};
			machineModelName?: string;
			machineModelVersion?: string;
		};
		const electronDevices = (raw.gpuDevice ?? []).map(normalizeElectronGpuDevice);
		return {
			devices: mergeGpuDevices(native?.devices ?? [], electronDevices),
			glRenderer: raw.auxAttributes?.glRenderer || undefined,
			glVendor: raw.auxAttributes?.glVendor || undefined,
			machineModelName: raw.machineModelName || undefined,
			machineModelVersion: raw.machineModelVersion || undefined,
			nativeSource: native?.source,
		};
	} catch {
		return {devices: native?.devices ?? [], nativeSource: native?.source};
	}
}

export function getAppMetricsSnapshot(): AppMetricsSnapshot {
	const cpus = os.cpus();
	const firstCpu = cpus[0];
	const cpuInfo: CpuInfo = {
		model: firstCpu?.model ?? 'Unknown',
		speed: firstCpu?.speed ?? 0,
		cores: cpus.length,
		physicalCores: cpus.length,
	};
	const metrics = app.getAppMetrics();
	const processes: Array<ProcessMetrics> = metrics.map((m) => ({
		cpu: {percentCPUUsage: m.cpu.percentCPUUsage},
		memory: {
			workingSetSize: m.memory.workingSetSize,
			peakWorkingSetSize: m.memory.peakWorkingSetSize,
			privateBytes: (m.memory as {privateBytes?: number}).privateBytes,
		},
		pid: m.pid,
		type: m.type,
		name: m.name,
	}));
	return {
		cpuInfo,
		processes,
		totalMemoryMB: Math.round(os.totalmem() / 1048576),
		freeMemoryMB: Math.round(os.freemem() / 1048576),
	};
}
