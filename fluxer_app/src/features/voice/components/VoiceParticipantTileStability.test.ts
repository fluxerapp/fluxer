// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {describe, expect, it} from 'vitest';

function sourceFile(name: string): string {
	return readFileSync(new URL(name, import.meta.url), 'utf8');
}

function appSourceFile(pathFromAppSrc: string): string {
	return readFileSync(new URL(`../../../${pathFromAppSrc}`, import.meta.url), 'utf8');
}

describe('VoiceParticipantTile stability', () => {
	it('sizes participant avatars on first paint without a resize observer or transform correction pass', () => {
		const tileSource = sourceFile('VoiceParticipantTile.tsx');
		const hookSource = sourceFile('voice_participant_tile/hooks.ts');
		const sharedSource = sourceFile('voice_participant_tile/shared.ts');
		const css = sourceFile('VoiceParticipantTile.module.css');
		expect(tileSource).not.toContain('useAvatarScale');
		expect(hookSource).not.toContain('useAvatarScale');
		expect(hookSource).not.toContain('--tile-avatar-scale');
		expect(sharedSource).not.toContain('resolveAvatarSize');
		expect(tileSource).toContain('styles.tileAvatarRing');
		expect(css).toContain('--tile-avatar-size');
		expect(css).toContain('.tileAvatarRing');
		expect(css).toContain('32cqw');
		expect(css).toContain('32cqh');
		expect(css).not.toContain('--tile-avatar-scale');
		expect(css).not.toMatch(/transform:\s*scale/);
		expect(css).not.toContain('will-change: transform');
	});
	it('drives the tile avatar animation from the speaking signal so animated avatars play without hover', () => {
		const tileSource = sourceFile('VoiceParticipantTile.tsx');
		const avatarElement = tileSource.match(/<Avatar\b[\s\S]*?\/>/)?.[0];
		expect(avatarElement).toBeDefined();
		const forceAnimateExpression = avatarElement?.match(/forceAnimate=\{([^}]+)\}/)?.[1]?.trim();
		expect(forceAnimateExpression).toBeDefined();
		const animateFlagSource =
			forceAnimateExpression === 'isActuallySpeaking'
				? 'isActuallySpeaking'
				: tileSource.match(new RegExp(`const ${forceAnimateExpression} = ([^;]+);`))?.[1];
		expect(animateFlagSource).toContain('isActuallySpeaking');
		expect(animateFlagSource).toContain('!Accessibility.useReducedMotion');
		expect(tileSource).toContain("import Accessibility from '@app/features/accessibility/state/Accessibility';");
		const afterAvatarElement = tileSource.slice(tileSource.indexOf(avatarElement!) + avatarElement!.length);
		const mediaNodeDeps = afterAvatarElement.match(/\}, \[([\s\S]*?)\]\);/)?.[1] ?? '';
		expect(mediaNodeDeps).toContain(forceAnimateExpression);
	});
	it('keeps the fullscreen call surface mounted while the media room catches up to a channel switch', () => {
		const voiceCallViewSource = sourceFile('VoiceCallView.tsx');
		const guildChannelViewSource = appSourceFile('features/channel/components/channel_view/GuildChannelView.tsx');
		expect(voiceCallViewSource).toContain('const VoiceCallPendingView');
		expect(voiceCallViewSource).toMatch(
			/if \(!hasValidRoomForVoiceCallView\(channel\)\) {\s+return \(\s+<VoiceCallPendingView/,
		);
		expect(voiceCallViewSource).not.toMatch(/if \(!hasValidRoomForVoiceCallView\(channel\)\) {\s+return null;/);
		expect(guildChannelViewSource).not.toContain('{isConnectedToThisChannel && room ? (');
	});
});
