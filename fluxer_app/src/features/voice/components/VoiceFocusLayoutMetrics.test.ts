// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {
	FOCUS_EXPANDED_MAIN_MIN_HEIGHT_PX,
	FOCUS_EXPANDED_MINI_COLUMN_RULES,
	FOCUS_EXPANDED_MINI_TILE_ASPECT_RATIO,
	getFocusExpandedMiniColumnCount,
	resolveFocusExpandedMainMetrics,
} from '@app/features/voice/components/VoiceFocusLayoutMetrics';
import {describe, expect, it} from 'vitest';

function sourceFile(name: string): string {
	return readFileSync(new URL(name, import.meta.url), 'utf8');
}

describe('VoiceFocusLayoutMetrics', () => {
	it('matches the intended expanded-participant-grid column breakpoints', () => {
		expect(getFocusExpandedMiniColumnCount(619)).toBe(1);
		expect(getFocusExpandedMiniColumnCount(620)).toBe(2);
		expect(getFocusExpandedMiniColumnCount(759)).toBe(2);
		expect(getFocusExpandedMiniColumnCount(760)).toBe(3);
		expect(getFocusExpandedMiniColumnCount(1039)).toBe(3);
		expect(getFocusExpandedMiniColumnCount(1040)).toBe(4);
	});
	it('keeps expanded-focus main height deterministic and never below its CSS minimum', () => {
		const widths = [320, 619, 620, 759, 760, 1039, 1040, 1440, 1920];
		const heights = [220, 360, 520, 720, 1080];
		for (const containerWidth of widths) {
			for (const containerHeight of heights) {
				const metrics = resolveFocusExpandedMainMetrics({containerWidth, containerHeight});
				expect(metrics.mainMaxHeight).toBeGreaterThanOrEqual(FOCUS_EXPANDED_MAIN_MIN_HEIGHT_PX);
				expect(metrics.miniRowHeight).toBeCloseTo(
					Math.min(metrics.miniTileMaxWidth, metrics.miniColumnWidth) / FOCUS_EXPANDED_MINI_TILE_ASPECT_RATIO,
					5,
				);
				expect(metrics.miniContentWidth).toBeLessThanOrEqual(metrics.miniSectionWidth);
				expect(
					metrics.miniColumnWidth * metrics.miniColumns + metrics.miniGridGap * (metrics.miniColumns - 1),
				).toBeLessThanOrEqual(metrics.miniContentWidth + 0.001);
			}
		}
	});
	it('keeps the helper constants synchronized with the focus layout CSS', () => {
		const css = sourceFile('VoiceCallView.module.css');
		for (const rule of FOCUS_EXPANDED_MINI_COLUMN_RULES) {
			expect(css).toContain(`@container (min-width: ${rule.minWidth}px)`);
			expect(css).toContain(`--focus-expanded-mini-columns: ${rule.columns};`);
		}
		expect(css).toContain(
			'--focus-expanded-reserved-bottom-height: calc(44px + 2.5rem + var(--focus-expanded-mini-row-height));',
		);
		expect(css).toContain(`--focus-expanded-main-max-height: max(${FOCUS_EXPANDED_MAIN_MIN_HEIGHT_PX}px`);
		expect(css).toContain(
			`min-height: min(${FOCUS_EXPANDED_MAIN_MIN_HEIGHT_PX}px, var(--focus-expanded-main-max-height));`,
		);
	});
});
