// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/ui/confetti/Confetti.module.css';
import {observer} from 'mobx-react-lite';
import {useEffect, useRef, useState} from 'react';

const PARTICLE_COUNT = 120;
const DURATION_MS = 1800;
const GRAVITY = 0.00055;
const DRAG = 0.0012;
const COLOR_TOKENS = ['--brand-primary', '--brand-secondary', '--brand-primary-light'] as const;

interface Particle {
	x: number;
	y: number;
	vx: number;
	vy: number;
	spin: number;
	angle: number;
	width: number;
	height: number;
	color: string;
}

function createParticles(width: number, colors: ReadonlyArray<string>): Array<Particle> {
	return Array.from({length: PARTICLE_COUNT}, (_, index) => {
		const direction = -Math.PI / 2 + (Math.random() - 0.5) * 1.6;
		const speed = 0.35 + Math.random() * 0.45;
		return {
			x: width / 2 + (Math.random() - 0.5) * width * 0.3,
			y: 0,
			vx: Math.cos(direction) * speed,
			vy: -Math.sin(direction) * speed * 0.4,
			spin: (Math.random() - 0.5) * 0.02,
			angle: Math.random() * Math.PI,
			width: 5 + Math.random() * 4,
			height: 8 + Math.random() * 6,
			color: colors[index % colors.length] ?? '#4641d9',
		};
	});
}

function readColors(element: HTMLElement): Array<string> {
	const computed = getComputedStyle(element);
	return COLOR_TOKENS.map((token) => computed.getPropertyValue(token).trim()).filter((value) => value.length > 0);
}

export const Confetti = observer(function Confetti() {
	const canvasRef = useRef<HTMLCanvasElement | null>(null);
	const [finished, setFinished] = useState(false);
	const reducedMotion = Accessibility.useReducedMotion;
	useEffect(() => {
		const canvas = canvasRef.current;
		const context = canvas?.getContext('2d');
		if (!canvas || !context || reducedMotion) return;
		const ratio = window.devicePixelRatio || 1;
		const {width, height} = canvas.getBoundingClientRect();
		canvas.width = Math.round(width * ratio);
		canvas.height = Math.round(height * ratio);
		context.scale(ratio, ratio);
		const particles = createParticles(width, readColors(canvas));
		const startedAt = performance.now();
		let previous = startedAt;
		let frame = 0;
		const draw = (now: number) => {
			const elapsed = now - startedAt;
			const step = Math.min(now - previous, 32);
			previous = now;
			context.clearRect(0, 0, width, height);
			context.globalAlpha = Math.max(0, 1 - Math.max(0, elapsed - DURATION_MS * 0.6) / (DURATION_MS * 0.4));
			for (const particle of particles) {
				particle.vx -= particle.vx * DRAG * step;
				particle.vy += GRAVITY * step;
				particle.x += particle.vx * step;
				particle.y += particle.vy * step;
				particle.angle += particle.spin * step;
				context.save();
				context.translate(particle.x, particle.y);
				context.rotate(particle.angle);
				context.fillStyle = particle.color;
				context.fillRect(
					-particle.width / 2,
					-particle.height / 2,
					particle.width,
					particle.height * Math.cos(particle.angle * 2),
				);
				context.restore();
			}
			if (elapsed < DURATION_MS) {
				frame = requestAnimationFrame(draw);
				return;
			}
			setFinished(true);
		};
		frame = requestAnimationFrame(draw);
		return () => cancelAnimationFrame(frame);
	}, [reducedMotion]);
	if (reducedMotion || finished) return null;
	return <canvas ref={canvasRef} className={styles.canvas} aria-hidden data-flx="ui.confetti.canvas" />;
});
