// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import styles from '@app/features/premium/components/plutonium_page/PlutoniumPage.module.css';
import type {PlutoniumAtomScene} from '@app/features/premium/components/plutonium_page/PlutoniumPageAtomScene';
import type {HeroArt} from '@app/features/premium/components/plutonium_page/PlutoniumPageMedia';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import Window from '@app/features/window/state/Window';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef, useState} from 'react';

const SOFTWARE_RENDERER = /swiftshader|llvmpipe|softpipe|software|basic render/i;
const NEAR_MARGIN = '200px';
const IDLE_TIMEOUT_MS = 3000;
const IDLE_FALLBACK_MS = 200;

let hardwareWebGL2: boolean | null = null;

function probeHardwareWebGL2(): boolean {
	if (!('WebGL2RenderingContext' in window) || !('IntersectionObserver' in window) || !('ResizeObserver' in window)) {
		return false;
	}
	try {
		const gl = document.createElement('canvas').getContext('webgl2', {failIfMajorPerformanceCaveat: true});
		if (!gl) return false;
		let renderer = String(gl.getParameter(gl.RENDERER));
		if (renderer === 'WebKit WebGL') {
			const info = gl.getExtension('WEBGL_debug_renderer_info');
			if (info) renderer = String(gl.getParameter(info.UNMASKED_RENDERER_WEBGL));
		}
		gl.getExtension('WEBGL_lose_context')?.loseContext();
		return !SOFTWARE_RENDERER.test(renderer);
	} catch {
		return false;
	}
}

function hasHardwareWebGL2(): boolean {
	hardwareWebGL2 ??= probeHardwareWebGL2();
	return hardwareWebGL2;
}

function whenIdle(callback: () => void): () => void {
	if (typeof window.requestIdleCallback === 'function') {
		const handle = window.requestIdleCallback(callback, {timeout: IDLE_TIMEOUT_MS});
		return () => window.cancelIdleCallback(handle);
	}
	const handle = window.setTimeout(callback, IDLE_FALLBACK_MS);
	return () => window.clearTimeout(handle);
}

interface PlutoniumPageAtomProps {
	art: HeroArt;
	live: boolean;
}

export const PlutoniumPageAtom = observer(function PlutoniumPageAtom({art, live}: PlutoniumPageAtomProps) {
	const hostRef = useRef<HTMLDivElement | null>(null);
	const imgRef = useRef<HTMLImageElement | null>(null);
	const sceneRef = useRef<PlutoniumAtomScene | null>(null);
	const [ready, setReady] = useState(false);
	const motionAllowed = live && !Accessibility.useReducedMotion;
	const active = Window.visible && (Window.focused || !isDesktop());
	const activeRef = useRef(active);

	useEffect(() => {
		const host = hostRef.current;
		if (!motionAllowed || ready || !host || !('IntersectionObserver' in window)) return;
		let cancelIdle: (() => void) | null = null;
		const observer = new IntersectionObserver(
			(entries) => {
				cancelIdle?.();
				cancelIdle = null;
				if (!entries[entries.length - 1].isIntersecting) return;
				cancelIdle = whenIdle(() => {
					if (hasHardwareWebGL2()) setReady(true);
				});
			},
			{rootMargin: NEAR_MARGIN},
		);
		observer.observe(host);
		return () => {
			observer.disconnect();
			cancelIdle?.();
		};
	}, [motionAllowed, ready]);

	useEffect(() => {
		const host = hostRef.current;
		const img = imgRef.current;
		if (!motionAllowed || !ready || !host || !img) return;
		let cancelled = false;
		loadLazyModule(
			() =>
				import(
					/* webpackChunkName: "plutonium-atom" */ '@app/features/premium/components/plutonium_page/PlutoniumPageAtomScene'
				),
		)
			.then(({mountPlutoniumAtom}) => {
				if (cancelled) return;
				sceneRef.current = mountPlutoniumAtom(host, img, {
					canvasClassName: styles.atomCanvas,
					active: activeRef.current,
				});
			})
			.catch(() => {});
		return () => {
			cancelled = true;
			sceneRef.current?.destroy();
			sceneRef.current = null;
		};
	}, [motionAllowed, ready]);

	useEffect(() => {
		activeRef.current = active;
		sceneRef.current?.setActive(active);
	}, [active]);

	return (
		<div ref={hostRef} className={styles.atomWrap} data-flx="premium.plutonium-page.plutonium-page.atom-wrap">
			<picture data-flx="premium.plutonium-page.plutonium-page.picture">
				<source
					type="image/avif"
					srcSet={art.avifSrcSet}
					data-flx="premium.plutonium-page.plutonium-page.source.image-avif"
				/>
				<source
					type="image/webp"
					srcSet={art.webpSrcSet}
					data-flx="premium.plutonium-page.plutonium-page.source.image-webp"
				/>
				<img
					ref={imgRef}
					draggable={false}
					className={styles.atom}
					src={art.src}
					width={art.width}
					height={art.height}
					alt=""
					decoding="sync"
					fetchPriority="high"
					data-flx="premium.plutonium-page.hero.atom"
				/>
			</picture>
		</div>
	);
});
