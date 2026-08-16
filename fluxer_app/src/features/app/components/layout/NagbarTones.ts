// SPDX-License-Identifier: AGPL-3.0-or-later

export interface NagbarTone {
	readonly backgroundColor: string;
	readonly textColor: string;
}

export const NagbarToneKind = Object.freeze({
	NEUTRAL: 'neutral',
	MAINTENANCE: 'maintenance',
	MAINTENANCE_SCHEDULED: 'maintenance_scheduled',
	MAINTENANCE_ACTIVE: 'maintenance_active',
	MAINTENANCE_COMPLETED: 'maintenance_completed',
	BRAND: 'brand',
	DANGER: 'danger',
	ALERT: 'alert',
	PREMIUM: 'premium',
	LEGAL: 'legal',
	VOICE: 'voice',
	CRITICAL: 'critical',
	DEVELOPMENT: 'development',
	STREAMER: 'streamer',
	ENCODER: 'encoder',
} as const);

export type NagbarToneKind = (typeof NagbarToneKind)[keyof typeof NagbarToneKind];

export const NAGBAR_TONES: Readonly<Record<NagbarToneKind, NagbarTone>> = Object.freeze({
	[NagbarToneKind.NEUTRAL]: Object.freeze({backgroundColor: '#4e5058', textColor: '#ffffff'}),
	[NagbarToneKind.MAINTENANCE]: Object.freeze({backgroundColor: '#4285f4', textColor: '#ffffff'}),
	[NagbarToneKind.MAINTENANCE_SCHEDULED]: Object.freeze({backgroundColor: '#1d4ed8', textColor: '#ffffff'}),
	[NagbarToneKind.MAINTENANCE_ACTIVE]: Object.freeze({backgroundColor: '#9a3412', textColor: '#ffffff'}),
	[NagbarToneKind.MAINTENANCE_COMPLETED]: Object.freeze({backgroundColor: '#166534', textColor: '#ffffff'}),
	[NagbarToneKind.BRAND]: Object.freeze({
		backgroundColor: 'var(--brand-primary)',
		textColor: 'var(--text-on-brand-primary)',
	}),
	[NagbarToneKind.DANGER]: Object.freeze({
		backgroundColor: 'var(--status-danger)',
		textColor: 'var(--text-on-brand-primary)',
	}),
	[NagbarToneKind.ALERT]: Object.freeze({backgroundColor: '#ea580c', textColor: '#ffffff'}),
	[NagbarToneKind.PREMIUM]: Object.freeze({backgroundColor: '#f97316', textColor: '#ffffff'}),
	[NagbarToneKind.LEGAL]: Object.freeze({backgroundColor: '#4338ca', textColor: '#ffffff'}),
	[NagbarToneKind.VOICE]: Object.freeze({backgroundColor: '#15803d', textColor: '#ffffff'}),
	[NagbarToneKind.CRITICAL]: Object.freeze({backgroundColor: '#b00000', textColor: '#ffffff'}),
	[NagbarToneKind.DEVELOPMENT]: Object.freeze({backgroundColor: '#17231d', textColor: '#72e6a2'}),
	[NagbarToneKind.STREAMER]: Object.freeze({backgroundColor: '#5865f2', textColor: '#ffffff'}),
	[NagbarToneKind.ENCODER]: Object.freeze({backgroundColor: '#b45309', textColor: '#ffffff'}),
});

export const NAGBAR_TONE_NEUTRAL: NagbarTone = NAGBAR_TONES[NagbarToneKind.NEUTRAL];
export const NAGBAR_TONE_MAINTENANCE: NagbarTone = NAGBAR_TONES[NagbarToneKind.MAINTENANCE];
