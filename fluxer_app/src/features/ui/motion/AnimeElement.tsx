// SPDX-License-Identifier: AGPL-3.0-or-later

import {type FlxElementName, flxElementClassName} from '@app/lib/react';
import {
	animate as animeAnimate,
	spring as animeSpring,
	cubicBezier,
	type JSAnimation,
	type Spring,
	type SpringParams,
} from 'animejs';
import React, {
	Children,
	createContext,
	forwardRef,
	isValidElement,
	useCallback,
	useContext,
	useEffect,
	useImperativeHandle,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
} from 'react';

type AnimeScalar = string | number;
type AnimeValueLike = AnimeValue<number> | AnimeValue<string>;
type AnimeStyleValue = AnimeScalar | AnimeValueLike | null | undefined;
type AnimeTargetValue = AnimeScalar | ReadonlyArray<AnimeScalar> | Readonly<Record<string, unknown>> | null;
type AnimeTransformRenderer = (transform: Readonly<Record<string, AnimeScalar>>) => string;
type AnimeEase = string | ((time: number) => number) | Spring;
type UnknownFunction = (...args: Array<never>) => unknown;
type MutableAnimeTarget = Record<string, AnimeTargetValue>;

const AnimeAnimationPhase = Object.freeze({
	ENTER: 'enter',
	EXIT: 'exit',
	HOVER: 'hover',
	PRESS: 'press',
} as const);
type AnimeAnimationPhase = (typeof AnimeAnimationPhase)[keyof typeof AnimeAnimationPhase];

export const AnimeTweenType = Object.freeze({
	SPRING: 'spring',
	TWEEN: 'tween',
} as const);
export type AnimeTweenType = (typeof AnimeTweenType)[keyof typeof AnimeTweenType];

export type AnimeTarget = Readonly<Record<string, AnimeTargetValue>>;
export interface AnimeTween {
	readonly type?: AnimeTweenType;
	readonly duration?: number;
	readonly delay?: number;
	readonly ease?: string | ReadonlyArray<number>;
	readonly stiffness?: number;
	readonly damping?: number;
	readonly mass?: number;
	readonly bounce?: number;
	readonly repeat?: number;
	readonly onUpdate?: (latest: number) => void;
	readonly onComplete?: () => void;
	readonly [key: string]: unknown;
}
export type AnimeStyle = Readonly<Record<string, AnimeStyleValue>>;
export type AnimeElementProps<TagName extends keyof React.JSX.IntrinsicElements> = Omit<
	React.JSX.IntrinsicElements[TagName],
	keyof AnimeElementAnimationProps | 'style'
> &
	AnimeElementAnimationProps & {readonly style?: AnimeStyle};
export interface AnimePlaybackControls {
	readonly stop: () => void;
	readonly cancel: () => void;
}
export interface AnimeValue<T = number> {
	readonly get: () => T;
	readonly set: (value: T) => void;
	readonly subscribe: (listener: (value: T) => void) => () => void;
}

interface ActiveAnimation {
	readonly phase: AnimeAnimationPhase;
	readonly key: string;
}

interface AnimeElementAnimationProps {
	readonly from?: AnimeTarget | false;
	readonly to?: AnimeTarget | false;
	readonly leave?: AnimeTarget | false;
	readonly tween?: AnimeTween;
	readonly hover?: AnimeTarget;
	readonly press?: AnimeTarget;
	readonly renderTransform?: AnimeTransformRenderer;
	readonly onAnimeStart?: () => void;
	readonly onAnimeComplete?: () => void;
}

type InternalAnimeElementProps = AnimeElementAnimationProps &
	React.HTMLAttributes<HTMLElement> &
	React.SVGAttributes<SVGElement> & {
		readonly style?: AnimeStyle;
	};

interface PresenceContextValue {
	readonly enterOnMount: boolean;
	readonly isPresent: boolean;
	readonly registerExitAnimation: () => () => void;
}

interface PresenceItem {
	readonly key: string;
	readonly element: React.ReactElement;
	readonly isPresent: boolean;
}

interface AnimePresenceProps {
	readonly children?: React.ReactNode;
	readonly enterOnMount?: boolean;
}

const AnimeTransformKey = Object.freeze({
	TRANSLATE_X: 'translateX',
	TRANSLATE_Y: 'translateY',
	SCALE: 'scale',
	SCALE_X: 'scaleX',
	SCALE_Y: 'scaleY',
	ROTATE: 'rotate',
	ROTATE_X: 'rotateX',
	ROTATE_Y: 'rotateY',
} as const);
type AnimeTransformKey = (typeof AnimeTransformKey)[keyof typeof AnimeTransformKey];

const AnimeEaseKeyword = Object.freeze({
	EASE_OUT: 'easeOut',
	EASE_IN: 'easeIn',
	EASE_IN_OUT: 'easeInOut',
} as const);

const AnimeEaseExpression = Object.freeze({
	OUT: 'out(3)',
	IN: 'in(3)',
	IN_OUT: 'inOut(3)',
} as const);

const ANIME_TRANSFORM_KEYS = Object.freeze([
	AnimeTransformKey.TRANSLATE_X,
	AnimeTransformKey.TRANSLATE_Y,
	AnimeTransformKey.SCALE,
	AnimeTransformKey.SCALE_X,
	AnimeTransformKey.SCALE_Y,
	AnimeTransformKey.ROTATE,
	AnimeTransformKey.ROTATE_X,
	AnimeTransformKey.ROTATE_Y,
] as const satisfies ReadonlyArray<AnimeTransformKey>);

type AssertNever<T extends never> = T;

export type AnimeTransformKeyCoverageGap = AssertNever<
	Exclude<AnimeTransformKey, (typeof ANIME_TRANSFORM_KEYS)[number]>
>;
const ROTATE_TRANSFORM_KEY_PREFIX = 'rotate';
const AUTO_SIZE_VALUE = 'auto';
const DEFAULT_ANIME_EASE: AnimeEase = AnimeEaseExpression.OUT;
const DEFAULT_TWEEN_DURATION_SECONDS = 0.2;
const MILLISECONDS_PER_SECOND = 1000;
const ANIME_SPRING_DEFAULT_STIFFNESS = 100;
const ANIME_SPRING_DEFAULT_DAMPING = 10;
const ANIME_SPRING_DEFAULT_MASS = 1;
const ANIME_SPRING_MINIMUM_MASS = 1;
const ANIME_SPRING_MAXIMUM_PARAMETER = 10_000;
const CUBIC_BEZIER_CONTROL_POINT_COUNT = 4;
const MAX_ANIME_SIGNATURE_DEPTH = 16;
const MAX_ANIME_SIGNATURE_NODES = 256;
const CSS_PIXEL_UNIT = 'px';
const CSS_DEGREE_UNIT = 'deg';
const EMPTY_ANIME_STYLE: AnimeStyle = Object.freeze({});
const EMPTY_PRESENCE_ITEMS: ReadonlyArray<PresenceItem> = Object.freeze([]);

const PresenceContext = createContext<PresenceContextValue | null>(null);

function isAnimeTransformKey(key: string): key is AnimeTransformKey {
	switch (key) {
		case AnimeTransformKey.TRANSLATE_X:
		case AnimeTransformKey.TRANSLATE_Y:
		case AnimeTransformKey.SCALE:
		case AnimeTransformKey.SCALE_X:
		case AnimeTransformKey.SCALE_Y:
		case AnimeTransformKey.ROTATE:
		case AnimeTransformKey.ROTATE_X:
		case AnimeTransformKey.ROTATE_Y:
			return true;
		default:
			return false;
	}
}

function isReservedAnimeTargetKey(key: string): boolean {
	return key === 'tween' || key === 'transitionEnd';
}

class FunctionSignatureIdentityOwner {
	private readonly identities = new WeakMap<UnknownFunction, number>();
	private nextIdentity = 1;

	get(value: UnknownFunction): number {
		const existing = this.identities.get(value);
		if (existing != null) return existing;
		const identity = this.nextIdentity;
		this.nextIdentity += 1;
		this.identities.set(value, identity);
		return identity;
	}
}

const functionSignatureIdentityOwner = new FunctionSignatureIdentityOwner();

interface ResolvePresenceElementKeyRequest {
	readonly child: React.ReactElement;
	readonly index: number;
}

interface ResolvePresenceEnterOnMountRequest {
	readonly hasMounted: boolean;
	readonly enterOnMount: boolean;
}

interface ResolveExitingPresenceItemsRequest {
	readonly currentExitingItems: ReadonlyArray<PresenceItem>;
	readonly nextPresentItems: ReadonlyArray<PresenceItem>;
	readonly previousPresentItems: ReadonlyArray<PresenceItem>;
}

interface AnimePresenceItemProps {
	readonly enterOnMount: boolean;
	readonly item: PresenceItem;
	readonly registerExitAnimation: (key: string) => () => void;
}

interface QueuePresenceExitCompletionRequest {
	readonly key: string;
	readonly state: PresenceExitStateOwner;
}

function getElementKey({child, index}: ResolvePresenceElementKeyRequest): string {
	const key = child.key;
	if (key == null) {
		return `__index_${index}`;
	}
	return String(key);
}

function freezePresenceItems(items: Array<PresenceItem>): ReadonlyArray<PresenceItem> {
	return Object.freeze(items);
}

function getPresenceItems(children: React.ReactNode): ReadonlyArray<PresenceItem> {
	const items: Array<PresenceItem> = [];
	const childNodes = Children.toArray(children);
	let elementIndex = 0;
	for (const child of childNodes) {
		if (!isValidElement(child)) continue;
		items.push(
			Object.freeze({
				key: getElementKey({child, index: elementIndex}),
				element: child,
				isPresent: true,
			}),
		);
		elementIndex += 1;
	}
	return freezePresenceItems(items);
}

function resolvePresenceEnterOnMount({hasMounted, enterOnMount}: ResolvePresenceEnterOnMountRequest): boolean {
	if (hasMounted) {
		return true;
	}
	return enterOnMount;
}

function resolveExitingPresenceItems({
	currentExitingItems,
	nextPresentItems,
	previousPresentItems,
}: ResolveExitingPresenceItemsRequest): ReadonlyArray<PresenceItem> {
	const nextPresentKeys = new Set(nextPresentItems.map((item) => item.key));
	const exitingItems: Array<PresenceItem> = [];
	const exitingKeys = new Set<string>();
	let changed = false;
	for (const item of currentExitingItems) {
		if (nextPresentKeys.has(item.key)) {
			changed = true;
			continue;
		}
		exitingItems.push(item);
		exitingKeys.add(item.key);
	}
	for (const previousItem of previousPresentItems) {
		if (nextPresentKeys.has(previousItem.key) || exitingKeys.has(previousItem.key)) {
			continue;
		}
		exitingItems.push(Object.freeze({...previousItem, isPresent: false}));
		exitingKeys.add(previousItem.key);
		changed = true;
	}
	if (!changed) {
		return currentExitingItems;
	}
	return freezePresenceItems(exitingItems);
}

function combinePresenceItems(
	presentItems: ReadonlyArray<PresenceItem>,
	exitingItems: ReadonlyArray<PresenceItem>,
): ReadonlyArray<PresenceItem> {
	if (exitingItems.length === 0) {
		return presentItems;
	}
	return freezePresenceItems([...presentItems, ...exitingItems]);
}

class PresenceExitStateOwner {
	private pendingCount = 0;
	private completionQueued = false;

	register(): void {
		this.pendingCount += 1;
	}

	completeRegistration(): boolean {
		if (this.pendingCount > 0) {
			this.pendingCount -= 1;
		}
		return this.pendingCount === 0;
	}

	queueCompletion(): boolean {
		if (this.completionQueued || this.pendingCount !== 0) {
			return false;
		}
		this.completionQueued = true;
		return true;
	}

	completeQueuedCheck(): void {
		this.completionQueued = false;
	}

	isIdle(): boolean {
		return this.pendingCount === 0;
	}
}

class PresenceExitOwner {
	private readonly states = new Map<string, PresenceExitStateOwner>();
	private readonly removeItem: (key: string) => void;
	private active = true;
	private generation = 0;

	constructor(removeItem: (key: string) => void) {
		this.removeItem = removeItem;
	}

	activate(): () => void {
		this.active = true;
		return () => {
			this.active = false;
			this.generation += 1;
			this.states.clear();
		};
	}

	restore(items: ReadonlyArray<PresenceItem>): void {
		for (const item of items) {
			this.states.delete(item.key);
		}
	}

	observe(items: ReadonlyArray<PresenceItem>): void {
		for (const item of items) {
			if (item.isPresent) continue;
			const state = this.getOrCreateState(item.key);
			this.queueCompletionCheck({key: item.key, state});
		}
	}

	register(key: string): () => void {
		const state = this.getOrCreateState(key);
		state.register();
		let complete = false;
		return () => {
			if (complete) return;
			complete = true;
			if (this.states.get(key) !== state) return;
			if (state.completeRegistration()) {
				this.queueCompletionCheck({key, state});
			}
		};
	}

	private getOrCreateState(key: string): PresenceExitStateOwner {
		const existing = this.states.get(key);
		if (existing != null) {
			return existing;
		}
		const state = new PresenceExitStateOwner();
		this.states.set(key, state);
		return state;
	}

	private queueCompletionCheck({key, state}: QueuePresenceExitCompletionRequest): void {
		if (!state.queueCompletion()) return;
		const generation = this.generation;
		queueMicrotask(() => {
			state.completeQueuedCheck();
			if (!this.active || this.generation !== generation) return;
			if (this.states.get(key) !== state || !state.isIdle()) return;
			this.states.delete(key);
			this.removeItem(key);
		});
	}
}

function AnimePresenceItem({enterOnMount, item, registerExitAnimation}: AnimePresenceItemProps): React.ReactElement {
	const registerItemExitAnimation = useCallback(
		(): (() => void) => registerExitAnimation(item.key),
		[item.key, registerExitAnimation],
	);
	const contextValue = useMemo<PresenceContextValue>(
		() =>
			Object.freeze({
				enterOnMount,
				isPresent: item.isPresent,
				registerExitAnimation: registerItemExitAnimation,
			}),
		[enterOnMount, item.isPresent, registerItemExitAnimation],
	);
	return <PresenceContext.Provider value={contextValue}>{item.element}</PresenceContext.Provider>;
}

export function AnimePresence({children, enterOnMount}: AnimePresenceProps): React.ReactElement {
	const nextPresentItems = useMemo(() => getPresenceItems(children), [children]);
	const [exitingItems, setExitingItems] = useState(EMPTY_PRESENCE_ITEMS);
	const previousPresentItemsRef = useRef(nextPresentItems);
	const [exitOwner] = useState(
		() =>
			new PresenceExitOwner((key) => {
				setExitingItems((currentItems) => {
					const nextItems = currentItems.filter((item) => item.key !== key);
					if (nextItems.length === currentItems.length) {
						return currentItems;
					}
					return freezePresenceItems(nextItems);
				});
			}),
	);
	const nextExitingItems = resolveExitingPresenceItems({
		currentExitingItems: exitingItems,
		nextPresentItems,
		previousPresentItems: previousPresentItemsRef.current,
	});
	const items = combinePresenceItems(nextPresentItems, nextExitingItems);
	const hasMountedRef = useRef(false);
	const registerExitAnimation = useCallback((key: string): (() => void) => exitOwner.register(key), [exitOwner]);

	useLayoutEffect(() => exitOwner.activate(), [exitOwner]);

	useLayoutEffect(() => {
		exitOwner.restore(nextPresentItems);
		previousPresentItemsRef.current = nextPresentItems;
		if (nextExitingItems !== exitingItems) {
			setExitingItems(nextExitingItems);
		}
	}, [exitOwner, exitingItems, nextExitingItems, nextPresentItems]);

	useLayoutEffect(() => {
		exitOwner.observe(nextExitingItems);
	}, [exitOwner, nextExitingItems]);

	useLayoutEffect(() => {
		hasMountedRef.current = true;
	}, []);
	let enterOnMountSetting = true;
	if (enterOnMount != null) {
		enterOnMountSetting = enterOnMount;
	}
	const shouldEnterOnMount = resolvePresenceEnterOnMount({
		hasMounted: hasMountedRef.current,
		enterOnMount: enterOnMountSetting,
	});

	return (
		<>
			{items.map((item) => (
				<AnimePresenceItem
					key={item.key}
					enterOnMount={shouldEnterOnMount}
					item={item}
					registerExitAnimation={registerExitAnimation}
					data-flx="ui.motion.anime-element.anime-presence.anime-presence-item"
				/>
			))}
		</>
	);
}

class AnimeReactiveValue<T> implements AnimeValue<T> {
	private current: T;
	private readonly listeners = new Set<(value: T) => void>();

	constructor(initialValue: T) {
		this.current = initialValue;
	}

	get(): T {
		return this.current;
	}

	set(value: T): void {
		if (Object.is(this.current, value)) return;
		this.current = value;
		let failures: Array<unknown> | null = null;
		for (const listener of this.listeners) {
			try {
				listener(value);
			} catch (error) {
				if (failures == null) {
					failures = [];
				}
				failures.push(error);
			}
		}
		if (failures != null) {
			throw new AggregateError(failures, 'Anime reactive value listeners failed');
		}
	}

	subscribe(listener: (value: T) => void): () => void {
		this.listeners.add(listener);
		return () => {
			this.listeners.delete(listener);
		};
	}
}

function isAnimeValue(value: unknown): value is AnimeValueLike {
	return (
		typeof value === 'object' &&
		value != null &&
		'get' in value &&
		'set' in value &&
		'subscribe' in value &&
		typeof (value as AnimeValueLike).get === 'function' &&
		typeof (value as AnimeValueLike).set === 'function' &&
		typeof (value as AnimeValueLike).subscribe === 'function'
	);
}

export function useAnimeValue<T = number>(initialValue: T): AnimeValue<T> {
	const valueRef = useRef<AnimeValue<T> | null>(null);
	if (valueRef.current == null) {
		valueRef.current = new AnimeReactiveValue(initialValue);
	}
	return valueRef.current;
}

export interface UseAnimeValueEventRequest<T> {
	readonly value: AnimeValue<T>;
	readonly listener: (latest: T) => void;
}

export function useAnimeValueEvent<T>({value, listener}: UseAnimeValueEventRequest<T>): void {
	useEffect(() => value.subscribe(listener), [listener, value]);
}

function isFiniteNumber(value: unknown): value is number {
	if (typeof value !== 'number') {
		return false;
	}
	return Number.isFinite(value);
}

interface ConvertTweenSecondsRequest {
	readonly setting: string;
	readonly value: unknown;
}

interface ResolveSpringParameterRequest {
	readonly setting: 'damping' | 'mass' | 'stiffness';
	readonly value: unknown;
}

function millisecondsFromSeconds({setting, value}: ConvertTweenSecondsRequest): number {
	if (!isFiniteNumber(value)) {
		throw new TypeError(`Anime tween ${setting} must be a finite number of seconds`);
	}
	if (value < 0) {
		throw new RangeError(`Anime tween ${setting} must not be negative`);
	}
	return value * MILLISECONDS_PER_SECOND;
}

function hasSpringParameters(tween: AnimeTween): boolean {
	return tween.stiffness != null || tween.damping != null || tween.mass != null || tween.bounce != null;
}

function hasPhysicalSpringParameters(tween: AnimeTween): boolean {
	return tween.stiffness != null || tween.damping != null || tween.mass != null;
}

function hasPerceptualSpringParameters(tween: AnimeTween): boolean {
	return tween.duration != null || tween.bounce != null;
}

function isSpringTween(tween: AnimeTween | null): boolean {
	if (tween == null) {
		return false;
	}
	const type = tween.type;
	if (type != null && type !== AnimeTweenType.SPRING && type !== AnimeTweenType.TWEEN) {
		throw new TypeError(`Unsupported Anime tween type: ${String(type)}`);
	}
	const hasSpringSettings = hasSpringParameters(tween);
	if (type === AnimeTweenType.TWEEN && hasSpringSettings) {
		throw new TypeError('Anime tween physics require a spring tween');
	}
	return type === AnimeTweenType.SPRING || hasSpringSettings;
}

function resolvePositiveSpringParameter({setting, value}: ResolveSpringParameterRequest): number {
	if (!isFiniteNumber(value)) {
		throw new TypeError(`Anime spring ${setting} must be a finite number`);
	}
	if (value <= 0) {
		throw new RangeError(`Anime spring ${setting} must be greater than zero`);
	}
	if (value > ANIME_SPRING_MAXIMUM_PARAMETER) {
		throw new RangeError(`Anime spring ${setting} must not exceed ${ANIME_SPRING_MAXIMUM_PARAMETER}`);
	}
	return value;
}

function resolveSpringParameters(tween: AnimeTween): SpringParams {
	const parameters: SpringParams = {};
	const hasPhysicalParameters = hasPhysicalSpringParameters(tween);
	if (hasPhysicalParameters && hasPerceptualSpringParameters(tween)) {
		throw new TypeError('Anime spring physical parameters cannot be combined with duration or bounce');
	}
	if (tween.duration != null) {
		const duration = millisecondsFromSeconds({setting: 'duration', value: tween.duration});
		if (duration === 0 || duration > ANIME_SPRING_MAXIMUM_PARAMETER) {
			throw new RangeError(
				`Anime spring duration must be greater than zero and not exceed ${ANIME_SPRING_MAXIMUM_PARAMETER} milliseconds`,
			);
		}
		parameters.duration = duration;
	}
	if (tween.bounce != null) {
		if (!isFiniteNumber(tween.bounce)) {
			throw new TypeError('Anime spring bounce must be a finite number');
		}
		if (tween.bounce < -1 || tween.bounce > 1) {
			throw new RangeError('Anime spring bounce must be between -1 and 1');
		}
		parameters.bounce = tween.bounce;
	}
	if (!hasPhysicalParameters) {
		return parameters;
	}
	let stiffness = ANIME_SPRING_DEFAULT_STIFFNESS;
	if (tween.stiffness != null) {
		stiffness = resolvePositiveSpringParameter({setting: 'stiffness', value: tween.stiffness});
	}
	let damping = ANIME_SPRING_DEFAULT_DAMPING;
	if (tween.damping != null) {
		damping = resolvePositiveSpringParameter({setting: 'damping', value: tween.damping});
	}
	let mass = ANIME_SPRING_DEFAULT_MASS;
	if (tween.mass != null) {
		mass = resolvePositiveSpringParameter({setting: 'mass', value: tween.mass});
	}
	if (mass < ANIME_SPRING_MINIMUM_MASS) {
		stiffness /= mass;
		damping /= mass;
		mass = ANIME_SPRING_MINIMUM_MASS;
	}
	if (stiffness > ANIME_SPRING_MAXIMUM_PARAMETER || damping > ANIME_SPRING_MAXIMUM_PARAMETER) {
		throw new RangeError('Anime spring mass normalization exceeded the supported physical parameter range');
	}
	parameters.stiffness = stiffness;
	parameters.damping = damping;
	parameters.mass = mass;
	return parameters;
}

function getTweenDuration(tween: AnimeTween | null): number {
	if (tween == null || tween.duration == null) {
		return DEFAULT_TWEEN_DURATION_SECONDS * MILLISECONDS_PER_SECOND;
	}
	return millisecondsFromSeconds({setting: 'duration', value: tween.duration});
}

function getTweenDelay(tween: AnimeTween | null): number {
	if (tween == null || tween.delay == null) {
		return 0;
	}
	return millisecondsFromSeconds({setting: 'delay', value: tween.delay});
}

function easeFromKeyword(ease: string): AnimeEase {
	if (ease === AnimeEaseKeyword.EASE_OUT) {
		return AnimeEaseExpression.OUT;
	}
	if (ease === AnimeEaseKeyword.EASE_IN) {
		return AnimeEaseExpression.IN;
	}
	if (ease === AnimeEaseKeyword.EASE_IN_OUT) {
		return AnimeEaseExpression.IN_OUT;
	}
	return ease;
}

function easeFromControlPoints(ease: ReadonlyArray<number>): AnimeEase {
	if (ease.length !== CUBIC_BEZIER_CONTROL_POINT_COUNT) {
		throw new RangeError(`Anime cubic Bézier easing requires ${CUBIC_BEZIER_CONTROL_POINT_COUNT} control points`);
	}
	const [x1, y1, x2, y2] = ease;
	if (!isFiniteNumber(x1) || !isFiniteNumber(y1)) {
		throw new TypeError('Anime cubic Bézier easing control points must be finite numbers');
	}
	if (!isFiniteNumber(x2) || !isFiniteNumber(y2)) {
		throw new TypeError('Anime cubic Bézier easing control points must be finite numbers');
	}
	if (x1 < 0 || x1 > 1 || x2 < 0 || x2 > 1) {
		throw new RangeError('Anime cubic Bézier easing x control points must be between 0 and 1');
	}
	return cubicBezier(x1, y1, x2, y2);
}

function getTweenEase(tween: AnimeTween | null): AnimeEase {
	if (tween == null) {
		return DEFAULT_ANIME_EASE;
	}
	if (isSpringTween(tween)) {
		if (tween.ease != null) {
			throw new TypeError('Anime spring tweens cannot also declare an easing curve');
		}
		return animeSpring(resolveSpringParameters(tween));
	}
	const ease = tween.ease;
	if (typeof ease === 'string') {
		return easeFromKeyword(ease);
	}
	if (Array.isArray(ease)) {
		return easeFromControlPoints(ease);
	}
	if (ease != null) {
		throw new TypeError('Anime tween ease must be a string or four finite cubic Bézier control points');
	}
	return DEFAULT_ANIME_EASE;
}

function asAnimeScalar(value: unknown): AnimeScalar | null {
	if (typeof value === 'string') {
		return value;
	}
	if (typeof value === 'number') {
		return value;
	}
	return null;
}

function unwrapAnimeValue(value: AnimeTargetValue | AnimeStyleValue): AnimeScalar | null {
	if (isAnimeValue(value)) {
		return asAnimeScalar(value.get());
	}
	return asAnimeScalar(value);
}

interface NormalizeTransformValueRequest {
	readonly key: AnimeTransformKey;
	readonly value: AnimeScalar;
}

interface BuildTransformRequest {
	readonly target: Readonly<Record<string, AnimeStyleValue | AnimeTargetValue>>;
	readonly baseTransform: string | null;
	readonly renderTransform: AnimeTransformRenderer | null;
}

interface AssignTargetRequest {
	readonly element: HTMLElement | SVGElement;
	readonly target: AnimeTarget;
	readonly baseTransform: string | null;
	readonly renderTransform: AnimeTransformRenderer | null;
}

function normalizeTransformValue({key, value}: NormalizeTransformValueRequest): string {
	if (typeof value !== 'number') {
		return value;
	}
	if (key === AnimeTransformKey.TRANSLATE_X || key === AnimeTransformKey.TRANSLATE_Y) {
		return `${value}${CSS_PIXEL_UNIT}`;
	}
	if (key.startsWith(ROTATE_TRANSFORM_KEY_PREFIX)) {
		return `${value}${CSS_DEGREE_UNIT}`;
	}
	return String(value);
}

function getTransformValues(
	target: Readonly<Record<string, AnimeStyleValue | AnimeTargetValue>>,
): Readonly<Record<string, AnimeScalar>> {
	const values: Record<string, AnimeScalar> = {};
	for (const key of ANIME_TRANSFORM_KEYS) {
		const value = unwrapAnimeValue(target[key]);
		if (value == null) {
			continue;
		}
		values[key] = value;
	}
	return values;
}

function buildTransform({target, baseTransform, renderTransform}: BuildTransformRequest): string {
	const transformValues = getTransformValues(target);
	if (renderTransform != null) {
		return renderTransform(transformValues);
	}
	const segments: Array<string> = [];
	if (baseTransform != null && baseTransform.length > 0) {
		segments.push(baseTransform);
	}
	for (const key of ANIME_TRANSFORM_KEYS) {
		const value = transformValues[key];
		if (value == null) {
			continue;
		}
		segments.push(`${key}(${normalizeTransformValue({key, value})})`);
	}
	return segments.join(' ');
}

interface SplitAnimeTarget {
	readonly animationTarget: MutableAnimeTarget;
	readonly transformTarget: MutableAnimeTarget;
}

function splitTarget(target: AnimeTarget): SplitAnimeTarget {
	const animationTarget: MutableAnimeTarget = {};
	const transformTarget: MutableAnimeTarget = {};
	for (const [key, value] of Object.entries(target)) {
		if (isReservedAnimeTargetKey(key)) continue;
		if (isAnimeTransformKey(key)) {
			transformTarget[key] = value;
		} else {
			animationTarget[key] = value;
		}
	}
	return {animationTarget, transformTarget};
}

function assignTarget({element, target, baseTransform, renderTransform}: AssignTargetRequest): void {
	const {animationTarget, transformTarget} = splitTarget(target);
	for (const [key, value] of Object.entries(animationTarget)) {
		const unwrapped = unwrapAnimeValue(value);
		if (unwrapped == null) continue;
		Reflect.set(element.style, key, String(unwrapped));
	}
	const transform = buildTransform({target: transformTarget, baseTransform, renderTransform});
	if (transform.length > 0) {
		element.style.transform = transform;
	}
}

function buildAnimeTarget(target: AnimeTarget): MutableAnimeTarget {
	const {animationTarget, transformTarget} = splitTarget(target);
	const animeTarget: MutableAnimeTarget = {...animationTarget};
	for (const key of ANIME_TRANSFORM_KEYS) {
		const value = transformTarget[key];
		if (value == null) {
			continue;
		}
		animeTarget[key] = value;
	}
	return animeTarget;
}

interface ResolvedAnimeElementSettings {
	readonly fromTarget: AnimeTarget | null;
	readonly enterTarget: AnimeTarget | null;
	readonly exitTarget: AnimeTarget | null;
	readonly hoverTarget: AnimeTarget | null;
	readonly pressTarget: AnimeTarget | null;
	readonly tween: AnimeTween | null;
	readonly renderTransform: AnimeTransformRenderer | null;
	readonly animeStyle: AnimeStyle;
}

function isAnimeTarget(value: unknown): value is AnimeTarget {
	if (value == null) {
		return false;
	}
	return typeof value === 'object';
}

function shouldSettleOnMount(from: unknown): boolean {
	return from === false;
}

function resolveAnimeElementSettings(props: InternalAnimeElementProps): ResolvedAnimeElementSettings {
	let fromTarget: AnimeTarget | null = null;
	if (isAnimeTarget(props.from)) {
		fromTarget = props.from;
	}
	let enterTarget: AnimeTarget | null = null;
	if (isAnimeTarget(props.to)) {
		enterTarget = props.to;
	}
	let exitTarget: AnimeTarget | null = null;
	if (isAnimeTarget(props.leave)) {
		exitTarget = props.leave;
	}
	let hoverTarget: AnimeTarget | null = null;
	if (isAnimeTarget(props.hover)) {
		hoverTarget = props.hover;
	}
	let pressTarget: AnimeTarget | null = null;
	if (isAnimeTarget(props.press)) {
		pressTarget = props.press;
	}
	let tween: AnimeTween | null = null;
	if (props.tween != null) {
		tween = props.tween;
	}
	let renderTransform: AnimeTransformRenderer | null = null;
	if (props.renderTransform != null) {
		renderTransform = props.renderTransform;
	}
	let animeStyle: AnimeStyle = EMPTY_ANIME_STYLE;
	if (props.style != null) {
		animeStyle = props.style;
	}
	return {fromTarget, enterTarget, exitTarget, hoverTarget, pressTarget, tween, renderTransform, animeStyle};
}

function getFunctionSignature(value: UnknownFunction): string {
	const id = functionSignatureIdentityOwner.get(value);
	return `function:${id}`;
}

class CyclicAnimeSignatureValueError extends Error {
	constructor() {
		super('Anime animation targets and tweens must be acyclic');
		this.name = 'CyclicAnimeSignatureValueError';
	}
}

class AnimeSignatureComplexityError extends Error {
	constructor() {
		super('Anime animation target or tween exceeds the signature complexity bound');
		this.name = 'AnimeSignatureComplexityError';
	}
}

class AnimeValueSignatureOwner {
	private readonly activeObjects = new Set<object>();
	private nodeCount = 0;

	get(value: unknown): string {
		return this.visit(value, 0);
	}

	private visit(value: unknown, depth: number): string {
		this.nodeCount += 1;
		if (this.nodeCount > MAX_ANIME_SIGNATURE_NODES || depth > MAX_ANIME_SIGNATURE_DEPTH) {
			throw new AnimeSignatureComplexityError();
		}
		if (typeof value === 'function') {
			return getFunctionSignature(value as UnknownFunction);
		}
		if (value == null) {
			return `${typeof value}:${String(value)}`;
		}
		switch (typeof value) {
			case 'string':
			case 'number':
			case 'boolean':
				return `${typeof value}:${String(value)}`;
		}
		if (typeof value !== 'object') {
			return `${typeof value}:${String(value)}`;
		}
		if (this.activeObjects.has(value)) {
			throw new CyclicAnimeSignatureValueError();
		}
		this.activeObjects.add(value);
		try {
			if (isAnimeValue(value)) {
				return `anime-value:${this.visit(value.get(), depth + 1)}`;
			}
			if (Array.isArray(value)) {
				return `[${value.map((item) => this.visit(item, depth + 1)).join(',')}]`;
			}
			const entries = Object.entries(value as Record<string, unknown>).sort(([left], [right]) =>
				left.localeCompare(right),
			);
			return `{${entries.map(([key, entryValue]) => `${key}:${this.visit(entryValue, depth + 1)}`).join(',')}}`;
		} finally {
			this.activeObjects.delete(value);
		}
	}
}

function getValueSignature(value: unknown): string {
	return new AnimeValueSignatureOwner().get(value);
}

function getTargetSignature(target: AnimeTarget): string {
	const signatureOwner = new AnimeValueSignatureOwner();
	const entries = Object.entries(target).sort(([left], [right]) => left.localeCompare(right));
	return entries.map(([key, value]) => `${key}:${signatureOwner.get(value)}`).join('|');
}

function baseTransformSignature(baseTransform: string | null): string {
	if (baseTransform == null) {
		return '';
	}
	return baseTransform;
}

function renderTransformSignature(renderTransform: AnimeTransformRenderer | null): string {
	if (renderTransform == null) {
		return '';
	}
	return getFunctionSignature(renderTransform);
}

interface AnimationKeyArgs {
	readonly phase: AnimeAnimationPhase;
	readonly target: AnimeTarget;
	readonly tween: AnimeTween | null;
	readonly baseTransform: string | null;
	readonly renderTransform: AnimeTransformRenderer | null;
}

function getAnimationKey({phase, target, tween, baseTransform, renderTransform}: AnimationKeyArgs): string {
	return [
		phase,
		getTargetSignature(target),
		getValueSignature(tween),
		baseTransformSignature(baseTransform),
		renderTransformSignature(renderTransform),
	].join('::');
}

const AutoSizeProperty = Object.freeze({
	HEIGHT: 'height',
	WIDTH: 'width',
} as const);

type AutoSizeProperty = (typeof AutoSizeProperty)[keyof typeof AutoSizeProperty];

const AUTO_SIZE_PROPERTIES: ReadonlyArray<AutoSizeProperty> = Object.freeze(Object.values(AutoSizeProperty));

interface MeasureAutoSizeRequest {
	readonly element: HTMLElement | SVGElement;
	readonly property: AutoSizeProperty;
}

interface ResolveElementTweenRequest {
	readonly target: AnimeTarget;
	readonly tween: AnimeTween | null;
}

interface ResolveAutoSizeCompletionRequest {
	readonly element: HTMLElement | SVGElement;
	readonly autoSizeProperties: ReadonlyArray<AutoSizeProperty>;
	readonly onComplete: (() => void) | null;
}

interface ApplyPropertyTweensRequest {
	readonly animeTarget: MutableAnimeTarget;
	readonly tween: AnimeTween | null;
}

interface RunElementAnimationRequest {
	readonly element: HTMLElement | SVGElement;
	readonly target: AnimeTarget;
	readonly tween: AnimeTween | null;
	readonly onComplete: () => void;
}

interface AnimateAnimeValueRequest {
	readonly value: AnimeValue<number>;
	readonly to: number;
	readonly tween: AnimeTween | null;
}

interface AnimateNumberRequest {
	readonly from: number;
	readonly to: number;
	readonly tween: AnimeTween | null;
}

export interface AnimateAnimeNumberRequest {
	readonly from: number;
	readonly to: number;
	readonly tween?: AnimeTween;
}

export interface AnimateAnimeReactiveValueRequest {
	readonly value: AnimeValue<number>;
	readonly to: number;
	readonly tween?: AnimeTween;
}

export type AnimateAnimeRequest = AnimateAnimeNumberRequest | AnimateAnimeReactiveValueRequest;

function measureAutoSize({element, property}: MeasureAutoSizeRequest): number {
	const style = (element as HTMLElement).style;
	const previousValue = style.getPropertyValue(property);
	style.setProperty(property, AUTO_SIZE_VALUE);
	const rect = element.getBoundingClientRect();
	style.setProperty(property, previousValue);
	if (property === AutoSizeProperty.HEIGHT) {
		return rect.height;
	}
	return rect.width;
}

function resolveElementTween({target, tween}: ResolveElementTweenRequest): AnimeTween | null {
	const targetTween = target.tween;
	if (typeof targetTween !== 'object') {
		return tween;
	}
	if (targetTween == null) {
		return tween;
	}
	return targetTween as AnimeTween;
}

function resolveAnimationLoop(tween: AnimeTween | null): number | boolean | null {
	if (tween == null) {
		return null;
	}
	if (tween.repeat === Number.POSITIVE_INFINITY) {
		return true;
	}
	if (typeof tween.repeat !== 'number') {
		return null;
	}
	if (tween.repeat > 0) {
		return tween.repeat;
	}
	return null;
}

function resolvePropertyTween(tween: AnimeTween | null, property: string): AnimeTween | null {
	if (tween == null) {
		return null;
	}
	const propertyTween = tween[property];
	if (propertyTween == null) {
		return null;
	}
	if (typeof propertyTween !== 'object' || Array.isArray(propertyTween)) {
		throw new TypeError(`Anime tween settings for ${property} must be an object`);
	}
	return propertyTween as AnimeTween;
}

function applyPropertyTweens({animeTarget, tween}: ApplyPropertyTweensRequest): void {
	for (const [property, value] of Object.entries(animeTarget)) {
		const propertyTween = resolvePropertyTween(tween, property);
		if (propertyTween == null) {
			continue;
		}
		animeTarget[property] = {
			to: value,
			duration: getTweenDuration(propertyTween),
			delay: getTweenDelay(propertyTween),
			ease: getTweenEase(propertyTween),
		};
	}
}

function resolveAutoSizeCompletion({
	element,
	autoSizeProperties,
	onComplete,
}: ResolveAutoSizeCompletionRequest): (() => void) | null {
	if (autoSizeProperties.length === 0) {
		return onComplete;
	}
	return () => {
		const style = (element as HTMLElement).style;
		for (const property of autoSizeProperties) {
			style.setProperty(property, AUTO_SIZE_VALUE);
		}
		if (onComplete != null) {
			onComplete();
		}
	};
}

function runElementAnimation({element, target, tween, onComplete}: RunElementAnimationRequest): AnimePlaybackControls {
	const animeTarget = buildAnimeTarget(target);
	if (Object.keys(animeTarget).length === 0) {
		onComplete();
		return noopControls;
	}
	const autoSizeProperties: Array<AutoSizeProperty> = [];
	for (const property of AUTO_SIZE_PROPERTIES) {
		if (animeTarget[property] === AUTO_SIZE_VALUE) {
			animeTarget[property] = `${measureAutoSize({element, property})}${CSS_PIXEL_UNIT}`;
			autoSizeProperties.push(property);
		}
	}
	const resolvedTween = resolveElementTween({target, tween});
	applyPropertyTweens({animeTarget, tween: resolvedTween});
	const completeAnimation = resolveAutoSizeCompletion({element, autoSizeProperties, onComplete});
	const params: Record<string, unknown> = {
		...animeTarget,
		duration: getTweenDuration(resolvedTween),
		delay: getTweenDelay(resolvedTween),
		ease: getTweenEase(resolvedTween),
	};
	const loop = resolveAnimationLoop(resolvedTween);
	if (loop != null) {
		params.loop = loop;
	}
	if (completeAnimation != null) {
		params.onComplete = completeAnimation;
	}
	const animation = animeAnimate(element, params as never);
	return createPlaybackControls(animation);
}

const noopControls: AnimePlaybackControls = Object.freeze({
	stop: () => {},
	cancel: () => {},
});

function createPlaybackControls(animation: JSAnimation): AnimePlaybackControls {
	const cancel = (): void => {
		animation.cancel();
	};
	return Object.freeze({stop: cancel, cancel});
}

function animateAnimeValue({value, to, tween}: AnimateAnimeValueRequest): AnimePlaybackControls {
	const state = {value: value.get()};
	const onUpdate = resolveTweenUpdateHandler(tween);
	const onComplete = resolveTweenCompleteHandler(tween);
	const animation = animeAnimate(state, {
		value: to,
		duration: getTweenDuration(tween),
		delay: getTweenDelay(tween),
		ease: getTweenEase(tween),
		onUpdate: () => {
			value.set(state.value);
			if (onUpdate != null) {
				onUpdate(state.value);
			}
		},
		onComplete: () => {
			value.set(to);
			if (onUpdate != null) {
				onUpdate(to);
			}
			if (onComplete != null) {
				onComplete();
			}
		},
	});
	return createPlaybackControls(animation);
}

function resolveTweenUpdateHandler(tween: AnimeTween | null): ((latest: number) => void) | null {
	if (tween == null) {
		return null;
	}
	if (typeof tween.onUpdate !== 'function') {
		return null;
	}
	return tween.onUpdate;
}

function resolveTweenCompleteHandler(tween: AnimeTween | null): (() => void) | null {
	if (tween == null) {
		return null;
	}
	if (typeof tween.onComplete !== 'function') {
		return null;
	}
	return tween.onComplete;
}

function animateNumber({from, to, tween}: AnimateNumberRequest): AnimePlaybackControls {
	const state = {value: from};
	const onUpdate = resolveTweenUpdateHandler(tween);
	const onComplete = resolveTweenCompleteHandler(tween);
	const animation = animeAnimate(state, {
		value: to,
		duration: getTweenDuration(tween),
		delay: getTweenDelay(tween),
		ease: getTweenEase(tween),
		onUpdate: () => {
			if (onUpdate != null) {
				onUpdate(state.value);
			}
		},
		onComplete: () => {
			if (onUpdate != null) {
				onUpdate(to);
			}
			if (onComplete != null) {
				onComplete();
			}
		},
	});
	return createPlaybackControls(animation);
}

export function animateAnime(request: AnimateAnimeRequest): AnimePlaybackControls {
	let tween: AnimeTween | null = null;
	if (request.tween != null) {
		tween = request.tween;
	}
	if ('from' in request) {
		return animateNumber({from: request.from, to: request.to, tween});
	}
	return animateAnimeValue({value: request.value, to: request.to, tween});
}

interface StartAnimationArgs {
	readonly phase: AnimeAnimationPhase;
	readonly element: HTMLElement | SVGElement;
	readonly target: AnimeTarget;
	readonly complete: (() => void) | null;
	readonly notifyStart: boolean;
	readonly tween?: AnimeTween | null;
}

const SETTLE_ON_MOUNT_TWEEN: AnimeTween = Object.freeze({duration: 0});

interface ActiveAnimationIdentity {
	readonly phase: AnimeAnimationPhase;
	readonly animationKey: string;
}

interface MatchActiveAnimationRequest extends ActiveAnimationIdentity {
	readonly activeAnimation: ActiveAnimation | null;
}

interface CreateAnimeElementRequest {
	readonly tagName: string;
	readonly normalizeClassName: ((className: string | null) => string) | null;
}

interface ResolveAnimationKeyRequest {
	readonly phase: AnimeAnimationPhase;
	readonly target: AnimeTarget;
}

interface StartPhaseAnimationRequest {
	readonly phase: AnimeAnimationPhase;
	readonly target: AnimeTarget | null;
}

interface RestoreAfterTapRequest {
	readonly event: React.PointerEvent<HTMLElement>;
	readonly handler: ((event: React.PointerEvent<HTMLElement>) => void) | null;
}

interface InitialiseFirstMountTargetRequest {
	readonly animateTarget: AnimeTarget | null;
	readonly animationOwner: AnimeElementAnimationOwner;
	readonly baseTransform: string | null;
	readonly element: HTMLElement | SVGElement;
	readonly fromTarget: AnimeTarget | null;
	readonly presence: PresenceContextValue | null;
	readonly renderTransform: AnimeTransformRenderer | null;
	readonly resolveAnimationKey: (request: ResolveAnimationKeyRequest) => string;
}

function matchesActiveAnimation({activeAnimation, phase, animationKey}: MatchActiveAnimationRequest): boolean {
	if (activeAnimation == null) {
		return false;
	}
	if (activeAnimation.phase !== phase) {
		return false;
	}
	return activeAnimation.key === animationKey;
}

class AnimeElementAnimationOwner {
	private animation: AnimePlaybackControls = noopControls;
	private activeAnimation: ActiveAnimation | null = null;
	private exitRegistration: (() => void) | null = null;
	private mounted = false;

	matches(request: ActiveAnimationIdentity): boolean {
		return matchesActiveAnimation({...request, activeAnimation: this.activeAnimation});
	}

	stopAnimation(): void {
		this.animation.stop();
		this.animation = noopControls;
		this.activeAnimation = null;
	}

	beginAnimation(activeAnimation: ActiveAnimation): void {
		this.activeAnimation = activeAnimation;
	}

	setAnimation(animation: AnimePlaybackControls): void {
		this.animation = animation;
	}

	completeAnimation({phase, animationKey}: ActiveAnimationIdentity): void {
		if (!this.matches({phase, animationKey})) return;
		this.animation = noopControls;
	}

	releaseExitRegistration(): void {
		const registration = this.exitRegistration;
		if (registration == null) return;
		this.exitRegistration = null;
		registration();
	}

	replaceExitRegistration(registration: () => void): void {
		const previousRegistration = this.exitRegistration;
		this.exitRegistration = registration;
		if (previousRegistration != null) {
			previousRegistration();
		}
	}

	completeExitRegistration(registration: () => void): void {
		if (this.exitRegistration === registration) {
			this.exitRegistration = null;
		}
		registration();
	}

	takeFirstMount(): boolean {
		if (this.mounted) {
			return false;
		}
		this.mounted = true;
		return true;
	}
}

function resolveBaseTransform(animeStyle: AnimeStyle): string | null {
	const transform = animeStyle.transform;
	if (typeof transform !== 'string') {
		return null;
	}
	return transform;
}

function resolveExistingClassName(value: unknown): string | null {
	if (typeof value !== 'string') {
		return null;
	}
	return value;
}

function shouldEnterOnMount(presence: PresenceContextValue | null): boolean {
	if (presence == null) {
		return true;
	}
	return presence.enterOnMount;
}

function initialiseFirstMountTarget({
	animateTarget,
	animationOwner,
	baseTransform,
	element,
	fromTarget,
	presence,
	renderTransform,
	resolveAnimationKey,
}: InitialiseFirstMountTargetRequest): boolean {
	if (shouldEnterOnMount(presence)) {
		if (fromTarget != null) assignTarget({element, target: fromTarget, baseTransform, renderTransform});
		return false;
	}
	if (animateTarget == null) return false;
	assignTarget({element, target: animateTarget, baseTransform, renderTransform});
	animationOwner.beginAnimation({
		phase: AnimeAnimationPhase.ENTER,
		key: resolveAnimationKey({phase: AnimeAnimationPhase.ENTER, target: animateTarget}),
	});
	return true;
}

type AnimeComponent<TagName extends keyof React.JSX.IntrinsicElements> = React.ForwardRefExoticComponent<
	AnimeElementProps<TagName> & React.RefAttributes<HTMLElement | SVGElement>
>;

function createAnimeElement({tagName, normalizeClassName}: CreateAnimeElementRequest) {
	return forwardRef<HTMLElement | SVGElement, InternalAnimeElementProps>(
		function AnimeElementComponent(props, forwardedRef) {
			const {
				from,
				to,
				leave,
				tween,
				hover,
				press,
				style,
				renderTransform,
				onAnimeStart,
				onAnimeComplete,
				onMouseEnter,
				onMouseLeave,
				onPointerDown,
				onPointerUp,
				onPointerCancel,
				...restProps
			} = props;
			const {
				fromTarget,
				enterTarget,
				exitTarget,
				hoverTarget,
				pressTarget,
				animeStyle,
				tween: elementTween,
				renderTransform: elementRenderTransform,
			} = resolveAnimeElementSettings(props);
			const elementRef = useRef<HTMLElement | SVGElement | null>(null);
			const [animationOwner] = useState(() => new AnimeElementAnimationOwner());
			const onAnimeStartRef = useRef(onAnimeStart);
			const onAnimeCompleteRef = useRef(onAnimeComplete);
			const presence = useContext(PresenceContext);
			const baseTransform = resolveBaseTransform(animeStyle);

			const animationKeyFor = ({phase, target}: ResolveAnimationKeyRequest): string =>
				getAnimationKey({
					phase,
					target,
					tween: elementTween,
					baseTransform,
					renderTransform: elementRenderTransform,
				});

			const startAnimation = ({phase, element, target, complete, notifyStart, tween}: StartAnimationArgs): void => {
				const animationKey = animationKeyFor({phase, target});
				if (animationOwner.matches({phase, animationKey})) {
					return;
				}
				if (phase !== AnimeAnimationPhase.EXIT) {
					animationOwner.releaseExitRegistration();
				}
				animationOwner.stopAnimation();
				animationOwner.beginAnimation({phase, key: animationKey});
				if (notifyStart) {
					const onAnimeStartHandler = onAnimeStartRef.current;
					if (onAnimeStartHandler != null) {
						onAnimeStartHandler();
					}
				}
				let resolvedStartTween = elementTween;
				if (tween !== undefined) {
					resolvedStartTween = tween;
				}
				animationOwner.setAnimation(
					runElementAnimation({
						element,
						target,
						tween: resolvedStartTween,
						onComplete: () => {
							animationOwner.completeAnimation({phase, animationKey});
							if (complete != null) {
								complete();
							}
						},
					}),
				);
			};

			const startPhaseAnimation = ({phase, target}: StartPhaseAnimationRequest): void => {
				const element = elementRef.current;
				if (element == null) {
					return;
				}
				if (target == null) {
					return;
				}
				startAnimation({phase, element, target, complete: null, notifyStart: false});
			};

			useImperativeHandle(forwardedRef, () => elementRef.current as HTMLElement | SVGElement, []);

			useLayoutEffect(() => {
				return () => {
					animationOwner.stopAnimation();
					animationOwner.releaseExitRegistration();
				};
			}, [animationOwner]);

			useLayoutEffect(() => {
				onAnimeStartRef.current = onAnimeStart;
				onAnimeCompleteRef.current = onAnimeComplete;
			}, [onAnimeComplete, onAnimeStart]);

			useLayoutEffect(() => {
				const subscriptions: Array<() => void> = [];
				const element = elementRef.current;
				if (element != null) {
					const applyAnimeStyle = () => {
						assignTarget({
							element,
							target: animeStyle as AnimeTarget,
							baseTransform,
							renderTransform: elementRenderTransform,
						});
					};
					applyAnimeStyle();
					for (const value of Object.values(animeStyle)) {
						if (!isAnimeValue(value)) continue;
						subscriptions.push(value.subscribe(applyAnimeStyle));
					}
				}
				return () => {
					for (const unsubscribe of subscriptions) {
						unsubscribe();
					}
				};
			}, [animeStyle, baseTransform, elementRenderTransform]);

			useLayoutEffect(() => {
				const element = elementRef.current;
				if (element == null) return;
				const animateTarget = enterTarget;
				if (presence != null && !presence.isPresent) {
					const leaveTarget = exitTarget;
					if (leaveTarget == null) {
						animationOwner.stopAnimation();
						animationOwner.replaceExitRegistration(presence.registerExitAnimation());
						animationOwner.releaseExitRegistration();
						return;
					}
					const exitAnimationKey = animationKeyFor({phase: AnimeAnimationPhase.EXIT, target: leaveTarget});
					if (animationOwner.matches({phase: AnimeAnimationPhase.EXIT, animationKey: exitAnimationKey})) {
						return;
					}
					const completeExit = presence.registerExitAnimation();
					animationOwner.replaceExitRegistration(completeExit);
					startAnimation({
						phase: AnimeAnimationPhase.EXIT,
						element,
						target: leaveTarget,
						complete: () => {
							animationOwner.completeExitRegistration(completeExit);
						},
						notifyStart: false,
					});
					return;
				}
				animationOwner.releaseExitRegistration();
				if (animationOwner.takeFirstMount()) {
					if (shouldSettleOnMount(from)) {
						if (animateTarget != null) {
							startAnimation({
								phase: AnimeAnimationPhase.ENTER,
								element,
								target: animateTarget,
								complete: null,
								notifyStart: false,
								tween: SETTLE_ON_MOUNT_TWEEN,
							});
							return;
						}
					} else {
						const firstMountTargetInitialised = initialiseFirstMountTarget({
							animateTarget,
							animationOwner,
							baseTransform,
							element,
							fromTarget,
							presence,
							renderTransform: elementRenderTransform,
							resolveAnimationKey: animationKeyFor,
						});
						if (firstMountTargetInitialised) return;
					}
				}
				if (animateTarget != null) {
					startAnimation({
						phase: AnimeAnimationPhase.ENTER,
						element,
						target: animateTarget,
						complete: () => {
							const onAnimeCompleteHandler = onAnimeCompleteRef.current;
							if (onAnimeCompleteHandler != null) {
								onAnimeCompleteHandler();
							}
						},
						notifyStart: true,
					});
					return;
				}
				animationOwner.stopAnimation();
			});

			const handleMouseEnter = (event: React.MouseEvent<HTMLElement>): void => {
				if (onMouseEnter != null) {
					onMouseEnter(event);
				}
				startPhaseAnimation({phase: AnimeAnimationPhase.HOVER, target: hoverTarget});
			};
			const handleMouseLeave = (event: React.MouseEvent<HTMLElement>): void => {
				if (onMouseLeave != null) {
					onMouseLeave(event);
				}
				startPhaseAnimation({phase: AnimeAnimationPhase.ENTER, target: enterTarget});
			};
			const handlePointerDown = (event: React.PointerEvent<HTMLElement>): void => {
				if (onPointerDown != null) {
					onPointerDown(event);
				}
				startPhaseAnimation({phase: AnimeAnimationPhase.PRESS, target: pressTarget});
			};
			const restoreAfterTap = ({event, handler}: RestoreAfterTapRequest): void => {
				if (handler != null) {
					handler(event);
				}
				startPhaseAnimation({phase: AnimeAnimationPhase.ENTER, target: enterTarget});
			};
			let pointerUpHandler: ((event: React.PointerEvent<HTMLElement>) => void) | null = null;
			if (onPointerUp != null) {
				pointerUpHandler = onPointerUp;
			}
			let pointerCancelHandler: ((event: React.PointerEvent<HTMLElement>) => void) | null = null;
			if (onPointerCancel != null) {
				pointerCancelHandler = onPointerCancel;
			}
			const domProps: Record<string, unknown> = {...restProps};
			if (normalizeClassName != null) {
				domProps.className = normalizeClassName(resolveExistingClassName(domProps.className));
			}
			const domStyle: React.CSSProperties = {};
			for (const [key, value] of Object.entries(animeStyle)) {
				if (isAnimeTransformKey(key) || isAnimeValue(value)) continue;
				(domStyle as Record<string, unknown>)[key] = value;
			}
			if (baseTransform != null && baseTransform.length > 0) {
				domStyle.transform = baseTransform;
			}
			return React.createElement(tagName, {
				...domProps,
				ref: elementRef,
				style: domStyle,
				onMouseEnter: handleMouseEnter,
				onMouseLeave: handleMouseLeave,
				onPointerDown: handlePointerDown,
				onPointerUp: (event: React.PointerEvent<HTMLElement>) => restoreAfterTap({event, handler: pointerUpHandler}),
				onPointerCancel: (event: React.PointerEvent<HTMLElement>) =>
					restoreAfterTap({event, handler: pointerCancelHandler}),
			});
		},
	);
}

export function createAnimeFlxElement<const Name extends FlxElementName>(name: Name): AnimeComponent<Name> {
	return createAnimeElement({tagName: name, normalizeClassName: flxElementClassName}) as AnimeComponent<Name>;
}

export const AnimeButton = createAnimeElement({
	tagName: 'button',
	normalizeClassName: null,
}) as AnimeComponent<'button'>;
export const AnimeImg = createAnimeElement({tagName: 'img', normalizeClassName: null}) as AnimeComponent<'img'>;
export const AnimeSpan = createAnimeElement({tagName: 'span', normalizeClassName: null}) as AnimeComponent<'span'>;
export const AnimeVideo = createAnimeElement({tagName: 'video', normalizeClassName: null}) as AnimeComponent<'video'>;
