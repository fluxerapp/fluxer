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

interface QueuedAnimation {
	readonly animation: string;
	readonly mirror: boolean;
	readonly holdFrame: number | null;
	remainingTicks: number;
	frame: number;
}

interface AnimationCycleStep {
	readonly animation: string;
	readonly ticks: number;
	readonly probability?: number;
	readonly frame?: number;
	readonly mirror?: boolean;
}

interface SmashPlayback {
	surface: SquishSurface;
	mirror: boolean;
	heavy: boolean;
	impactAt: number | null;
}

type SquishSurface = 'wall' | 'ceiling' | 'floor';

const SQUISH_SPRITES: Record<SquishSurface, {readonly small: string; readonly large: string}> = {
	wall: {small: 'wallSquishSmall', large: 'wallSquishLarge'},
	ceiling: {small: 'ceilingSquishSmall', large: 'ceilingSquishLarge'},
	floor: {small: 'floorSquishSmall', large: 'floorSquishLarge'},
};

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
	lookN: [[-8, -2]],
	lookS: [[-8, -3]],
	lookE: [[-8, -1]],
	lookW: [[-8, 0]],
	wallSquishSmall: [[-9, 0]],
	wallSquishLarge: [[-9, -1]],
	ceilingSquishSmall: [[-9, -2]],
	ceilingSquishLarge: [[-9, -3]],
	floorSquishSmall: [[-10, -1]],
	floorSquishLarge: [[-10, -2]],
	sulk: [
		[-10, -2],
		[-10, -3],
	],
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
const SULK_NEKO_ANIMATION_THRESHOLD = 0.7;
const WALL_SQUISH_HEAVY_SPEED = 1.5;
const WALL_SQUISH_WINDUP_MS = 120;
const WALL_SQUISH_SMALL_HOLD_MS = 200;
const WALL_SQUISH_LARGE_HOLD_MS = 350;
const WALL_SLOW_NEAR_MS = 140;
const WALL_SLOW_SCALE = 0.08;
const WALL_SLOW_EXIT_MS = 200;

const clamp = (value: number, min: number, max: number): number => Math.min(Math.max(value, min), max);

interface AxisStep {
	readonly position: number;
	readonly velocity: number;
	readonly bounced: boolean;
	readonly wall: 'min' | 'max' | null;
}

const reflectAxis = (position: number, velocity: number, delta: number, min: number, max: number): AxisStep => {
	const next = position + velocity * delta;
	if (next < min) {
		return {
			position: clamp(min + (min - next) * BOUNCE_FRICTION, min, max),
			velocity: -velocity * BOUNCE_FRICTION,
			bounced: true,
			wall: 'min',
		};
	}
	if (next > max) {
		return {
			position: clamp(max - (next - max) * BOUNCE_FRICTION, min, max),
			velocity: -velocity * BOUNCE_FRICTION,
			bounced: true,
			wall: 'max',
		};
	}
	return {position: next, velocity, bounced: false, wall: null};
};

const DECAY_PER_MS = Math.log(FRICTION) / 16.67;

const timeToBoundMs = (position: number, velocity: number, min: number, max: number): number => {
	if (velocity === 0) return Number.POSITIVE_INFINITY;
	const distance = velocity > 0 ? max - position : position - min;
	const reach = 1 + (distance * DECAY_PER_MS) / Math.abs(velocity);
	if (reach <= 0) return Number.POSITIVE_INFINITY;
	return Math.log(reach) / DECAY_PER_MS;
};

interface PredictedWallHit {
	readonly timeToImpactMs: number;
	readonly surface: SquishSurface;
	readonly mirror: boolean;
}

const predictWallHit = (
	positionX: number,
	positionY: number,
	velocityX: number,
	velocityY: number,
	minX: number,
	maxX: number,
	minY: number,
	maxY: number,
): PredictedWallHit | null => {
	const timeX = timeToBoundMs(positionX, velocityX, minX, maxX);
	const timeY = timeToBoundMs(positionY, velocityY, minY, maxY);
	const timeToImpactMs = Math.min(timeX, timeY);
	if (!Number.isFinite(timeToImpactMs)) return null;
	if (timeX <= timeY) {
		return {timeToImpactMs, surface: 'wall', mirror: velocityX < 0};
	}
	return {timeToImpactMs, surface: velocityY < 0 ? 'ceiling' : 'floor', mirror: false};
};

const approachTimeScale = (simRemainingMs: number): number => {
	if (!Number.isFinite(simRemainingMs) || simRemainingMs >= WALL_SLOW_NEAR_MS) return 1;
	const closeness = Math.max(0, simRemainingMs) / WALL_SLOW_NEAR_MS;
	return WALL_SLOW_SCALE + (1 - WALL_SLOW_SCALE) * closeness;
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
		let smash: SmashPlayback | null = null;
		let slowRelease: {from: number; startedAt: number} | null = null;
		let didBounce = false;
		const animationQueue: Array<QueuedAnimation> = [];

		const setSprite = (name: string, frame: number): void => {
			const set = SPRITE_SETS[name] ?? SPRITE_SETS.idle;
			const sprite = set[frame % set.length];
			neko.style.backgroundPosition = `${sprite[0] * NEKO_CELL}px ${sprite[1] * NEKO_CELL}px`;
		};

		const setPosition = (): void => {
			neko.style.left = `${nekoPosX - NEKO_HALF}px`;
			neko.style.top = `${nekoPosY - NEKO_HALF}px`;
		};

		const setMirrored = (mirror: boolean): void => {
			neko.style.transform = mirror ? 'scaleX(-1)' : '';
		};

		const enqueueAnimation = (
			animation: string,
			ticks: number,
			options: {mirror?: boolean | undefined; frame?: number | undefined} = {},
		): void => {
			animationQueue.push({
				animation,
				mirror: options.mirror ?? false,
				holdFrame: options.frame ?? null,
				remainingTicks: Math.max(1, ticks),
				frame: 0,
			});
		};

		const runAnimationCycle = (steps: ReadonlyArray<AnimationCycleStep>, probability = 1): void => {
			if (Math.random() > probability) return;
			for (const step of steps) {
				if (Math.random() > (step.probability ?? 1)) continue;
				enqueueAnimation(step.animation, step.ticks, {mirror: step.mirror, frame: step.frame});
			}
		};

		const playQueuedAnimation = (): boolean => {
			const current = animationQueue[0];
			if (current == null) return false;
			setSprite(current.animation, current.holdFrame ?? current.frame);
			setMirrored(current.mirror);
			current.frame += 1;
			current.remainingTicks -= 1;
			if (current.remainingTicks <= 0) {
				animationQueue.shift();
				if (animationQueue.length === 0) setMirrored(false);
			}
			return true;
		};

		const setFlyingSprite = (): void => {
			const {vx, vy} = velocityRef.current;
			const xDirection = vx > 0 ? 'E' : 'W';
			const yDirection = vy > 0 ? 'S' : 'N';
			const direction = Math.abs(vx) > Math.abs(vy) ? xDirection : yDirection;
			setMirrored(false);
			setSprite(`look${direction}`, 0);
		};

		const drawSmash = (timestamp: number): boolean => {
			if (smash == null) return false;
			const squish = SQUISH_SPRITES[smash.surface];
			if (smash.impactAt == null) {
				setSprite(squish.small, 0);
				setMirrored(smash.mirror);
				return true;
			}
			const hold = smash.heavy ? WALL_SQUISH_LARGE_HOLD_MS : WALL_SQUISH_SMALL_HOLD_MS;
			if (timestamp - smash.impactAt >= hold) {
				smash = null;
				return false;
			}
			setSprite(smash.heavy ? squish.large : squish.small, 0);
			setMirrored(smash.mirror);
			return true;
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

		const updateMomentum = (timestamp: number, dt: number): void => {
			const frameDt = Math.min(dt, MAX_FRAME_DT);
			const maxX = Math.max(NEKO_HALF, window.innerWidth - NEKO_HALF);
			const maxY = Math.max(NEKO_HALF, window.innerHeight - NEKO_HALF);

			const speed = Math.hypot(velocityRef.current.vx, velocityRef.current.vy);
			const prediction = predictWallHit(
				nekoPosX,
				nekoPosY,
				velocityRef.current.vx,
				velocityRef.current.vy,
				NEKO_HALF,
				maxX,
				NEKO_HALF,
				maxY,
			);
			const timeToWall = prediction?.timeToImpactMs ?? Number.POSITIVE_INFINITY;
			const heavy = speed >= WALL_SQUISH_HEAVY_SPEED;

			let timeScale = 1;
			if (slowRelease != null) {
				const progress = clamp((timestamp - slowRelease.startedAt) / WALL_SLOW_EXIT_MS, 0, 1);
				timeScale = slowRelease.from + (1 - slowRelease.from) * progress;
				if (progress >= 1) slowRelease = null;
			}
			timeScale = Math.min(timeScale, approachTimeScale(timeToWall));
			const simDt = frameDt * timeScale;

			if (smash == null && prediction != null && heavy) {
				if (timeToWall / timeScale <= WALL_SQUISH_WINDUP_MS) {
					smash = {surface: prediction.surface, mirror: prediction.mirror, heavy: true, impactAt: null};
				}
			}

			const momentumDecay = FRICTION ** (simDt / 16.67);
			velocityRef.current.vx *= momentumDecay;
			velocityRef.current.vy *= momentumDecay;

			const impactVx = Math.abs(velocityRef.current.vx);
			const impactVy = Math.abs(velocityRef.current.vy);
			const impactSpeed = Math.hypot(impactVx, impactVy);
			const nextX = reflectAxis(nekoPosX, velocityRef.current.vx, simDt, NEKO_HALF, maxX);
			const nextY = reflectAxis(nekoPosY, velocityRef.current.vy, simDt, NEKO_HALF, maxY);
			nekoPosX = nextX.position;
			nekoPosY = nextY.position;
			velocityRef.current.vx = nextX.velocity;
			velocityRef.current.vy = nextY.velocity;
			setPosition();

			if (nextX.bounced || nextY.bounced) {
				didBounce = true;
				if (timeScale < 1) slowRelease = {from: timeScale, startedAt: timestamp};
				const onWall = nextX.bounced && (!nextY.bounced || impactVx >= impactVy);
				let surface: SquishSurface = 'floor';
				if (onWall) surface = 'wall';
				else if (nextY.wall === 'min') surface = 'ceiling';
				const mirror = onWall && nextX.wall === 'min';
				if (smash == null) {
					smash = {surface, mirror, heavy: impactSpeed >= WALL_SQUISH_HEAVY_SPEED, impactAt: timestamp};
				} else {
					smash.impactAt = timestamp;
					smash.surface = surface;
					smash.mirror = mirror;
				}
			}

			if (Math.hypot(velocityRef.current.vx, velocityRef.current.vy) < STOP_SPEED) {
				isFlingingRef.current = false;
				velocityRef.current = {vx: 0, vy: 0};
				didFlingRef.current = true;
				smash = null;
				if (didBounce) {
					const finalSulkTicks = 10 + Math.floor(Math.random() * 16);
					runAnimationCycle(
						[
							{animation: 'sulk', ticks: 4, frame: 0},
							{animation: 'sulk', ticks: finalSulkTicks, frame: 1},
						],
						SULK_NEKO_ANIMATION_THRESHOLD,
					);
				}
				return;
			}

			if (!drawSmash(timestamp)) setFlyingSprite();
		};

		const tick = (timestamp: number): void => {
			const dt = lastTimestamp === null ? 16.67 : timestamp - lastTimestamp;
			lastTimestamp = timestamp;

			if (isFlingingRef.current) {
				updateMomentum(timestamp, dt);
			}
			if (timestamp - lastFrameTimestamp >= FRAME_INTERVAL) {
				lastFrameTimestamp = timestamp;
				if (!playQueuedAnimation()) {
					if (isDragging) {
						setSprite('alert', 0);
					} else if (isFlingingRef.current) {
						if (smash == null) setFlyingSprite();
					} else if (reducedMotionRef.current) {
						setSprite('idle', 0);
					} else if (didFlingRef.current) {
						idle();
					} else {
						step();
					}
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
			animationQueue.length = 0;
			smash = null;
			slowRelease = null;
			didBounce = false;
			setMirrored(false);
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
			isFlingingRef.current = !stale && fling.scalar > MIN_FLING_SPEED;
			smash = null;
			slowRelease = null;
			didBounce = false;
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
