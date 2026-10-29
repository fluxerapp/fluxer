// SPDX-License-Identifier: AGPL-3.0-or-later AND MIT
// SPDX-FileCopyrightText: 2022 adryd

import styles from '@app/features/accessibility/components/NekoSprite.module.css';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import nekoSpriteUrl from '@app/media/images/neko.png';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useRef} from 'react';

type SpriteFrame = readonly [number, number];
type DragSample = {x: number; y: number; t: number};
type Cycle = {animation: SpriteFrame; probability: number; next?: number};
type AnimationCycles = Array<Cycle>;
const SPRITE_SETS: Record<string, ReadonlyArray<SpriteFrame>> = {
	idle: [[-3, -3]],
	alert: [[-7, -3]],
	tired: [[-3, -2]],
	sleeping: [
		[-2, 0],
		[-2, -1],
	],
	scratchSelf: [
		[-5, 0],
		[-6, 0],
		[-7, 0],
	],
	scratchWallN: [
		[0, 0],
		[0, -1],
	],
	scratchWallS: [
		[-7, -1],
		[-6, -2],
	],
	scratchWallE: [
		[-2, -2],
		[-2, -3],
	],
	scratchWallW: [
		[-4, 0],
		[-4, -1],
	],
	// TODO:
	lookN: [],
	lookS: [],
	lookE: [],
	lookW: [],
	wallSmashSmall: [],
	wallSmashLarge: [],
	N: [
		[-1, -2],
		[-1, -3],
	],
	NE: [
		[0, -2],
		[0, -3],
	],
	E: [
		[-3, 0],
		[-3, -1],
	],
	SE: [
		[-5, -1],
		[-5, -2],
	],
	S: [
		[-6, -3],
		[-7, -2],
	],
	SW: [
		[-5, -3],
		[-6, -1],
	],
	W: [
		[-4, -2],
		[-4, -3],
	],
	NW: [
		[-1, 0],
		[-1, -1],
	],
};

const NEKO_CELL = 32;
const NEKO_HALF = 16;
const NEKO_SPEED = 10;
const FRAME_INTERVAL = 100;
const IDLE_DISTANCE = 48;
const DRAG_THRESHOLD = 6;
const FRICTION = 0.9;
const STOP_SPEED = 0.02;
const BOUNCE_FRICTION = 0.85;
const MIN_FLING_SPEED = 0.05;
const MAX_FLING_SPEED = 5;
const FLING_SAMPLE_WINDOW_MS = 100;
const FLING_STALE_MS = 80;
const MAX_FRAME_DT = 32;
const SAD_NEKO_ANIMATION_THRESHOLD = 0.7;

const clamp = (value: number, min: number, max: number): number => Math.min(Math.max(value, min), max);

interface AxisStep {
	readonly position: number;
	readonly velocity: number;
}

const reflectAxis = (position: number, velocity: number, delta: number, min: number, max: number): AxisStep => {
	const next = position + velocity * delta;
	if (next < min) {
		return {
			position: clamp(min + (min - next) * BOUNCE_FRICTION, min, max),
			velocity: -velocity * BOUNCE_FRICTION,
		};
	}
	if (next > max) {
		return {
			position: clamp(max - (next - max) * BOUNCE_FRICTION, min, max),
			velocity: -velocity * BOUNCE_FRICTION,
		};
	}
	return {position: next, velocity};
};

const NekoEngine = observer((): React.ReactElement => {
	const reducedMotion = Accessibility.useReducedMotion;
	const keepNekoStill = Accessibility.keepNekoStill;
	const reducedMotionRef = useRef(reducedMotion);
	const keepNekoStillRef = useRef(keepNekoStill);
	reducedMotionRef.current = reducedMotion;
	keepNekoStillRef.current = keepNekoStill;
	const nekoRef = useRef<HTMLDivElement | null>(null);
	const dragBufferRef = useRef<Array<DragSample>>([]);
	const velocityRef = useRef({vx: 0, vy: 0});
	const isFlingingRef = useRef(false);
	const didFlingRef = useRef(false);

	useEffect(() => {
		const neko = nekoRef.current;
		if (!neko) return;

		let nekoPosX = clamp(window.innerWidth - 48, NEKO_HALF, window.innerWidth - NEKO_HALF);
		let nekoPosY = clamp(window.innerHeight - 96, NEKO_HALF, window.innerHeight - NEKO_HALF);
		let mousePosX = nekoPosX;
		let mousePosY = nekoPosY;
		let frameCount = 0;
		let idleTime = 0;
		let idleAnimation: string | null = null;
		let idleAnimationFrame = 0;
		let lastTimestamp: number | null = null;
		let lastFrameTimestamp = 0;
		let rafId = 0;
		let isDragging = false;
		let dragPointerId: number | null = null;
		let pointerDownX = 0;
		let pointerDownY = 0;

		const setSprite = (name: string, frame: number): void => {
			const set = SPRITE_SETS[name] ?? SPRITE_SETS.idle;
			const sprite = set[frame % set.length];
			neko.style.backgroundPosition = `${sprite[0] * NEKO_CELL}px ${sprite[1] * NEKO_CELL}px`;
		};

		const setPosition = (): void => {
			neko.style.left = `${nekoPosX - NEKO_HALF}px`;
			neko.style.top = `${nekoPosY - NEKO_HALF}px`;
		};

		const resetIdleAnimation = (): void => {
			idleAnimation = null;
			idleAnimationFrame = 0;
		};

		const idle = (): void => {
			idleTime += 1;
			if (idleTime > 10 && Math.random() < 0.005 && idleAnimation == null) {
				const available = ['sleeping', 'scratchSelf'];
				if (nekoPosX < NEKO_CELL) available.push('scratchWallW');
				if (nekoPosY < NEKO_CELL) available.push('scratchWallN');
				if (nekoPosX > window.innerWidth - NEKO_CELL) available.push('scratchWallE');
				if (nekoPosY > window.innerHeight - NEKO_CELL) available.push('scratchWallS');
				idleAnimation = available[Math.floor(Math.random() * available.length)];
			}
			switch (idleAnimation) {
				case 'sleeping':
					if (idleAnimationFrame < 8) {
						setSprite('tired', 0);
						break;
					}
					setSprite('sleeping', Math.floor(idleAnimationFrame / 4));
					if (idleAnimationFrame > 192) resetIdleAnimation();
					break;
				case 'scratchWallN':
				case 'scratchWallS':
				case 'scratchWallE':
				case 'scratchWallW':
				case 'scratchSelf':
					setSprite(idleAnimation, idleAnimationFrame);
					if (idleAnimationFrame > 9) resetIdleAnimation();
					break;
				default:
					setSprite('idle', 0);
					return;
			}
			idleAnimationFrame += 1;
		};

		const step = (): void => {
			if (keepNekoStillRef.current) {
				idle();
				return;
			}
			frameCount += 1;
			const diffX = nekoPosX - mousePosX;
			const diffY = nekoPosY - mousePosY;
			const distance = Math.sqrt(diffX * diffX + diffY * diffY);
			if (distance < NEKO_SPEED || distance < IDLE_DISTANCE) {
				idle();
				return;
			}
			resetIdleAnimation();
			if (idleTime > 1) {
				setSprite('alert', 0);
				idleTime = Math.min(idleTime, 7) - 1;
				return;
			}
			let direction = '';
			direction += diffY / distance > 0.5 ? 'N' : '';
			direction += diffY / distance < -0.5 ? 'S' : '';
			direction += diffX / distance > 0.5 ? 'W' : '';
			direction += diffX / distance < -0.5 ? 'E' : '';
			setSprite(direction, frameCount);
			nekoPosX -= (diffX / distance) * NEKO_SPEED;
			nekoPosY -= (diffY / distance) * NEKO_SPEED;
			nekoPosX = clamp(nekoPosX, NEKO_HALF, window.innerWidth - NEKO_HALF);
			nekoPosY = clamp(nekoPosY, NEKO_HALF, window.innerHeight - NEKO_HALF);
			setPosition();
		};

		const updateMomentum = (dt: number): void => {
			const momentumDecay = FRICTION ** (dt / 16.67);
			velocityRef.current.vx *= momentumDecay;
			velocityRef.current.vy *= momentumDecay;

			const frameDt = Math.min(dt, MAX_FRAME_DT);
			const maxX = Math.max(NEKO_HALF, window.innerWidth - NEKO_HALF);
			const maxY = Math.max(NEKO_HALF, window.innerHeight - NEKO_HALF);
			const nextX = reflectAxis(nekoPosX, velocityRef.current.vx, frameDt, NEKO_HALF, maxX);
			const nextY = reflectAxis(nekoPosY, velocityRef.current.vy, frameDt, NEKO_HALF, maxY);
			nekoPosX = nextX.position;
			nekoPosY = nextY.position;
			velocityRef.current.vx = nextX.velocity;
			velocityRef.current.vy = nextY.velocity;
			setPosition();

			if (Math.hypot(velocityRef.current.vx, velocityRef.current.vy) < STOP_SPEED) {
				isFlingingRef.current = false;
				velocityRef.current = {vx: 0, vy: 0};
				didFlingRef.current = true;
			}
		};

		const tick = (timestamp: number): void => {
			const dt = lastTimestamp === null ? 16.67 : timestamp - lastTimestamp;
			lastTimestamp = timestamp;

			if (isFlingingRef.current) {
				updateMomentum(dt);
			}
			if (timestamp - lastFrameTimestamp >= FRAME_INTERVAL) {
				lastFrameTimestamp = timestamp;
				if (isDragging) {
					setSprite('alert', 0);
				} else if (isFlingingRef.current) {
					// TODO: sprites for sling direction, also FIX: only do this once before a bounce has happened
					const {vx, vy} = velocityRef.current;
					const xCondition = vx > 0 ? 'E' : 'W';
					const yCondition = vy > 0 ? 'S' : 'N';
					const sprite = Math.abs(vx) > Math.abs(vy) ? xCondition : yCondition;
					setSprite(sprite, 0);
				} else if (reducedMotionRef.current) {
					setSprite('idle', 0);
				} else if (didFlingRef.current) {
					idle();
				} else {
					step();
				}
			}
			rafId = requestAnimationFrame(tick);
		};

		const spawnHeart = (): void => {
			const heart = document.createElement('span');
			heart.className = styles.heart;
			heart.textContent = '♥';
			heart.setAttribute('aria-hidden', 'true');
			const cleanupHeart = (): void => heart.remove();
			heart.addEventListener('animationend', cleanupHeart, {once: true});
			window.setTimeout(cleanupHeart, 1500);
			neko.appendChild(heart);
		};

		const pet = (): void => {
			idleTime = 0;
			resetIdleAnimation();
			setSprite('alert', 0);
			spawnHeart();
		};

		const cycle = (animations: AnimationCycles): Promise<void> => {
			// TODO: potentially queue up set of animations to play
		};

		const onDocumentPointerMove = (event: PointerEvent): void => {
			if (isDragging) return;
			mousePosX = event.clientX;
			mousePosY = event.clientY;
		};

		const onNekoPointerDown = (event: PointerEvent): void => {
			event.preventDefault();
			isFlingingRef.current = false;
			didFlingRef.current = false;
			velocityRef.current = {vx: 0, vy: 0};
			dragBufferRef.current = [];
			dragPointerId = event.pointerId;
			pointerDownX = event.clientX;
			pointerDownY = event.clientY;
			isDragging = false;
			neko.setPointerCapture(event.pointerId);
		};

		const onNekoPointerMove = (event: PointerEvent): void => {
			if (dragPointerId !== event.pointerId) return;
			if (!isDragging) {
				const movedX = event.clientX - pointerDownX;
				const movedY = event.clientY - pointerDownY;
				if (Math.sqrt(movedX * movedX + movedY * movedY) <= DRAG_THRESHOLD) return;
				isDragging = true;
				resetIdleAnimation();
				idleTime = 0;
			}
			nekoPosX = clamp(event.clientX, NEKO_HALF, window.innerWidth - NEKO_HALF);
			nekoPosY = clamp(event.clientY, NEKO_HALF, window.innerHeight - NEKO_HALF);
			mousePosX = nekoPosX;
			mousePosY = nekoPosY;
			dragBufferRef.current.push({x: event.clientX, y: event.clientY, t: event.timeStamp});
			if (dragBufferRef.current.length > 5) {
				dragBufferRef.current.shift();
			}
			setPosition();
		};

		const computeVelocity = (
			buffer: Array<DragSample>,
			releaseTimestamp: number,
		): {vx: number; vy: number; scalar: number} => {
			const recent = buffer.filter((sample) => releaseTimestamp - sample.t <= FLING_SAMPLE_WINDOW_MS);
			if (recent.length < 2) return {vx: 0, vy: 0, scalar: 0};
			const initial = recent[0];
			const final = recent[recent.length - 1];
			const dt = final.t - initial.t;
			if (dt <= 0) return {vx: 0, vy: 0, scalar: 0};
			let vx = (final.x - initial.x) / dt;
			let vy = (final.y - initial.y) / dt;
			// TODO: perhaps make speed logarithmic so people can really sling it but you have to go supa hard with the mouse to get max speed
			const scalar = Math.sqrt(vx ** 2 + vy ** 2);
			if (scalar > MAX_FLING_SPEED) {
				const scale = MAX_FLING_SPEED / scalar;
				vx *= scale;
				vy *= scale;
				return {vx, vy, scalar: MAX_FLING_SPEED};
			}
			return {vx, vy, scalar};
		};

		const isDragBufferStale = (buffer: Array<DragSample>, releaseTimestamp: number): boolean => {
			const newest = buffer[buffer.length - 1];
			return newest == null || releaseTimestamp - newest.t > FLING_STALE_MS;
		};

		const endDrag = (event: PointerEvent, petOnTap: boolean): void => {
			if (dragPointerId !== event.pointerId) return;
			if (neko.hasPointerCapture(event.pointerId)) neko.releasePointerCapture(event.pointerId);
			const wasDragging = isDragging;
			if (petOnTap && !wasDragging) pet();
			isDragging = false;
			const fling = wasDragging ? computeVelocity(dragBufferRef.current, event.timeStamp) : {vx: 0, vy: 0, scalar: 0};
			const stale = isDragBufferStale(dragBufferRef.current, event.timeStamp);
			velocityRef.current = stale ? {vx: 0, vy: 0} : {vx: fling.vx, vy: fling.vy};
			// TODO: below play a small bump sprite instead of flinging.
			isFlingingRef.current = !stale && fling.scalar > MIN_FLING_SPEED;
			dragBufferRef.current = [];
			dragPointerId = null;
		};

		const onNekoPointerUp = (event: PointerEvent): void => endDrag(event, true);
		const onNekoPointerCancel = (event: PointerEvent): void => endDrag(event, false);

		const onResize = (): void => {
			nekoPosX = clamp(nekoPosX, NEKO_HALF, window.innerWidth - NEKO_HALF);
			nekoPosY = clamp(nekoPosY, NEKO_HALF, window.innerHeight - NEKO_HALF);
			setPosition();
		};

		neko.style.transform = '';
		setPosition();
		setSprite('idle', 0);
		document.addEventListener('pointermove', onDocumentPointerMove, {passive: true});
		neko.addEventListener('pointerdown', onNekoPointerDown);
		neko.addEventListener('pointermove', onNekoPointerMove);
		neko.addEventListener('pointerup', onNekoPointerUp);
		neko.addEventListener('pointercancel', onNekoPointerCancel);
		window.addEventListener('resize', onResize);
		rafId = requestAnimationFrame(tick);

		return () => {
			cancelAnimationFrame(rafId);
			document.removeEventListener('pointermove', onDocumentPointerMove);
			neko.removeEventListener('pointerdown', onNekoPointerDown);
			neko.removeEventListener('pointermove', onNekoPointerMove);
			neko.removeEventListener('pointerup', onNekoPointerUp);
			neko.removeEventListener('pointercancel', onNekoPointerCancel);
			window.removeEventListener('resize', onResize);
			neko.querySelectorAll(`.${styles.heart}`).forEach((heart) => heart.remove());
		};
	}, []);

	return (
		<div
			ref={nekoRef}
			className={styles.neko}
			style={{backgroundImage: `url(${nekoSpriteUrl})`}}
			aria-hidden="true"
			data-flx="accessibility.neko-sprite.neko"
		/>
	);
});

export const NekoSprite = observer((): React.ReactElement | null => {
	if (!Accessibility.showNeko) return null;
	return <NekoEngine data-flx="accessibility.neko-sprite.neko-engine" />;
});
