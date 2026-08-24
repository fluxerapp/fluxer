// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {describe, expect, it} from 'vitest';

function sourceFile(relativePath: string): string {
	return readFileSync(new URL(relativePath, import.meta.url), 'utf8');
}

describe('media viewer pan zoom stability', () => {
	it('keeps the custom pan-zoom surface independent from message observables', () => {
		const surfaceSource = sourceFile('pan_zoom/PanZoomSurface.tsx');
		const hookSource = sourceFile('pan_zoom/usePanZoomSurface.ts');
		const viewerSource = sourceFile('MediaViewers.tsx');
		expect(`${surfaceSource}\n${hookSource}`).not.toMatch(/mobx|mobx-react-lite|observer\(/);
		expect(viewerSource).toMatch(/memo\(\s*forwardRef<[^>]+>\(function DesktopMediaViewer/);
		expect(viewerSource).toMatch(/memo\(\s*forwardRef<[^>]+>\(function MobileMediaViewer/);
		expect(viewerSource).not.toMatch(/observer\(/);
	});
	it('does not reintroduce the removed zoom dependency at the media viewer call sites', () => {
		const mediaViewerSource = sourceFile('MediaViewers.tsx');
		const mobileVideoSource = sourceFile('../../../../voice/components/modals/MobileVideoViewer.tsx');
		const removedPackageName = ['react', 'zoom', 'pan', 'pinch'].join('-');
		expect(mediaViewerSource).not.toContain(removedPackageName);
		expect(mobileVideoSource).not.toContain(removedPackageName);
	});
	it('keeps the media viewport stable while zoom state changes', () => {
		const modalCss = sourceFile('../MediaModal.module.css');
		expect(modalCss).not.toMatch(/data-zoom-state=['"]zoomed['"][^{]*\.mediaArea/);
	});
	it('keeps parent mobile actions out of the fullscreen mobile video player', () => {
		const modalSource = sourceFile('../MediaModal.tsx');
		expect(modalSource).toContain('const isMobileVideo = Boolean(isMobile && mediaType ===');
		expect(modalSource).toContain('{isMobile && !isMobileVideo ? (');
	});
	it('stamps the pan-zoom measured-box marker on the rotation container', () => {
		const hookSource = sourceFile('pan_zoom/usePanZoomSurface.ts');
		const modalSource = sourceFile('../MediaModal.tsx');
		const selectorMatch = hookSource.match(/MEASURED_BOX_SELECTOR = '\[([a-z-]+)\]'/);
		expect(selectorMatch).not.toBeNull();
		const markerAttribute = selectorMatch?.[1] ?? 'missing-marker';
		expect(modalSource).toMatch(new RegExp(`styles\\.mediaContainer[^<]*\\b${markerAttribute}=`));
	});
	it('floors the media-fit clamps with a length-typed zero so they resolve to valid lengths', () => {
		const modalCss = sourceFile('../MediaModal.module.css');
		expect(modalCss).toMatch(/--media-fit-max-width:\s*max\(0px,/);
		expect(modalCss).toMatch(/--media-fit-max-height:\s*max\(0px,/);
		expect(modalCss).not.toMatch(/max\(\s*0\s*,/);
	});
});
