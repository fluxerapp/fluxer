// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ExternalAudioProcessorMatch {
	key: string;
	name: string;
}

const PROCESSORS: ReadonlyArray<{
	key: string;
	name: string;
	pattern: RegExp;
}> = [
	{key: 'nvidia-broadcast', name: 'NVIDIA Broadcast', pattern: /nvidia\s*broadcast|rtx\s*voice/i},
	{key: 'krisp', name: 'Krisp', pattern: /\bkrisp\b/i},
	{key: 'elgato-wave-link', name: 'Elgato Wave Link', pattern: /wave\s*link|elgato\s*wave/i},
	{key: 'voicemeeter', name: 'VoiceMeeter', pattern: /voicemeeter|vb-audio/i},
	{key: 'steelseries-sonar', name: 'SteelSeries Sonar', pattern: /steelseries\s*sonar|sonar\s*-\s*(mic|stream|chat)/i},
	{key: 'razer-synapse', name: 'Razer Synapse', pattern: /razer\s*virtual|synapse\s*virtual/i},
];

function detectExternalAudioProcessor(label: string | null | undefined): ExternalAudioProcessorMatch | null {
	if (!label) return null;
	for (const entry of PROCESSORS) {
		if (entry.pattern.test(label)) return {key: entry.key, name: entry.name};
	}
	return null;
}

export function findExternalProcessorForDevice(
	deviceId: string,
	devices: ReadonlyArray<MediaDeviceInfo>,
): ExternalAudioProcessorMatch | null {
	const device = devices.find((d) => d.deviceId === deviceId);
	return detectExternalAudioProcessor(device?.label);
}
