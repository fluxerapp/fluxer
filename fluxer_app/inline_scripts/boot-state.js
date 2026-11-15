(() => {
	try {
		if (
			location.protocol === 'fluxer-app:' ||
			window.electron ||
			/Electron\//.test(navigator.userAgent) ||
			!location.pathname.startsWith('/channels/')
		) {
			return;
		}
		if (typeof indexedDB === 'undefined' || !crypto.subtle || typeof DecompressionStream !== 'function') {
			return;
		}
		const frameIndexedDB = indexedDB;
		const format = '{{BOOT_STATE_FORMAT}}';
		const build = '{{BOOT_FRAME_BUILD}}';
		const accountKey = localStorage.getItem('fluxer:auth:active-account-key');
		const marker = JSON.parse(localStorage.getItem('{{BOOT_STATE_MARKER_STORAGE_KEY}}') || 'null');
		if (!accountKey || marker?.f !== format || !Array.isArray(marker.a) || !marker.a.includes(accountKey)) {
			return;
		}
		const root = document.documentElement;
		const encoder = new TextEncoder();
		const settle = (request) =>
			new Promise((resolve, reject) => {
				request.onsuccess = () => resolve(request.result);
				request.onerror = () => reject(request.error);
			});
		const inflate = async (key, sealed, additionalData) => {
			const plain = await crypto.subtle.decrypt(
				{name: 'AES-GCM', iv: sealed.iv, additionalData: encoder.encode(additionalData)},
				key,
				sealed.ciphertext,
			);
			return new Response(
				new Blob([plain]).stream().pipeThrough(new DecompressionStream('{{BOOT_STATE_COMPRESSION}}')),
			).arrayBuffer();
		};
		const frameState = {released: false, shown: null, input: null};
		const timers = new Set();
		const cleanups = new Set();
		let openGate;
		const later = (task, ms) => {
			const timer = setTimeout(() => {
				timers.delete(timer);
				task();
			}, ms);
			timers.add(timer);
			return timer;
		};
		const removeFrame = (preserve = false) => {
			const shown = frameState.shown;
			frameState.released = true;
			frameState.shown = null;
			for (const timer of timers) clearTimeout(timer);
			timers.clear();
			for (const cleanup of cleanups) cleanup();
			cleanups.clear();
			openGate?.();
			if (shown === null) {
				return null;
			}
			if (preserve === true) frameState.input = shown.take();
			shown.stopHolding();
			shown.container.remove();
			for (const node of shown.styles) node.remove();
			root.removeAttribute('{{BOOT_FRAME_INPUT_HELD_ATTRIBUTE}}');
			return shown;
		};
		later(() => removeFrame(true), Number('{{BOOT_FRAME_MAX_HOLD_MS}}'));
		const settled = new Promise((resolve) => {
			openGate = resolve;
		});
		later(openGate, Number('{{BOOT_FRAME_GATE_TIMEOUT_MS}}') - performance.now());
		window['{{BOOT_FRAME_GLOBAL}}'] = {
			settled,
			drop: () => {
				removeFrame();
				frameState.input = null;
			},
			needsHistory: () => frameState.shown?.needsHistory() === true,
			whenInputSettled: () => frameState.shown?.whenInputSettled() || Promise.resolve(),
			release: () => {
				const shown = frameState.shown;
				const held = shown === null ? frameState.input : shown.take();
				frameState.input = null;
				removeFrame();
				return held;
			},
		};
		const loaded = (link) =>
			new Promise((resolve, reject) => {
				const cleanup = () => {
					link.onload = null;
					link.onerror = null;
					cleanups.delete(cleanup);
				};
				link.onload = () => {
					cleanup();
					resolve();
				};
				link.onerror = () => {
					cleanup();
					reject();
				};
				cleanups.add(cleanup);
			});
		const whenRoot = () =>
			new Promise((resolve) => {
				const found = document.getElementById('root');
				if (found) {
					resolve(found);
					return;
				}
				const ready = () => {
					cleanup();
					resolve(document.getElementById('root'));
				};
				const cleanup = () => {
					document.removeEventListener('DOMContentLoaded', ready);
					cleanups.delete(cleanup);
				};
				cleanups.add(cleanup);
				document.addEventListener('DOMContentLoaded', ready, {once: true});
			});
		const holdInput = (container) => {
			const composer = container.querySelector('{{BOOT_FRAME_TEXT_TARGET_SELECTOR}}');
			const placeholder = container.querySelector('{{BOOT_FRAME_COMPOSER_PLACEHOLDER_SELECTOR}}');
			const inputs = [];
			const positions = new Map();
			const listeners = [];
			let composing = false;
			let pointerHeld = false;
			let pointerTimer;
			let finishComposition = null;
			let compositionTimer;
			let overflow = 0;
			const listen = (node, type, listener, options = true) => {
				node.addEventListener(type, listener, options);
				listeners.push(() => node.removeEventListener(type, listener, options));
			};
			const sameFlx = (owner, flx) => Array.from(owner.querySelectorAll(`[data-flx="${CSS.escape(flx)}"]`));
			const targetIdentity = (target) => {
				const element = target instanceof Element ? target : null;
				const owner = element?.closest('{{BOOT_FRAME_CLICK_IDENTITY_SELECTOR}}');
				const attribute = owner
					? Array.from(owner.attributes).find((item) =>
							'{{BOOT_FRAME_CLICK_IDENTITY_SELECTOR}}'.includes(`[${item.name}]`),
						)
					: null;
				const flxOwner = element?.closest('[data-flx]');
				const flx = flxOwner?.getAttribute('data-flx') || null;
				return {
					flx,
					index: flx === null ? -1 : sameFlx(container, flx).indexOf(flxOwner),
					identity: attribute ? [attribute.name, attribute.value] : null,
					innerIndex: owner && flx !== null ? sameFlx(owner, flx).indexOf(flxOwner) : -1,
					composer: !!composer && composer.contains(element),
				};
			};
			const modifiers = (event) => ({
				shiftKey: event.shiftKey,
				ctrlKey: event.ctrlKey,
				altKey: event.altKey,
				metaKey: event.metaKey,
			});
			const record = (input) => {
				const previousComposer =
					input.kind === 'composer' ? inputs.findLast((entry) => entry.kind === 'composer') : null;
				if (previousComposer && JSON.stringify(previousComposer) === JSON.stringify(input)) return;
				const previous = inputs.at(-1);
				if (input.kind === 'composer' && previous?.kind === 'composer') {
					inputs[inputs.length - 1] = input;
					return;
				}
				if (inputs.length >= Number('{{BOOT_FRAME_MAX_REPLAYED_INPUTS}}') - 1) {
					overflow += 1;
					return;
				}
				inputs.push(input);
				root.setAttribute('{{BOOT_FRAME_INPUT_HELD_ATTRIBUTE}}', '');
			};
			const lineText = (node) => {
				if (node.nodeType === 3) return node.textContent;
				return Array.from(node.childNodes, (child) => {
					if (child.nodeName === 'BR') return child.nextSibling ? '\n' : '';
					return lineText(child);
				}).join('');
			};
			const composerText = () => (composer ? Array.from(composer.childNodes, lineText).join('\n') : '');
			const initialLength = composerText().length;
			const selectionOffset = (node, offset) => {
				if (!composer || !node || !composer.contains(node)) return null;
				const range = document.createRange();
				range.setStart(composer, 0);
				range.setEnd(node, offset);
				const fragment = range.cloneContents();
				return Array.from(fragment.childNodes, lineText).join('\n').length;
			};
			const composerState = () => {
				const selection = document.getSelection();
				return {
					kind: 'composer',
					text: composerText(),
					anchor: selectionOffset(selection?.anchorNode, selection?.anchorOffset),
					focus: selectionOffset(selection?.focusNode, selection?.focusOffset),
					focused: !!composer && composer.contains(document.activeElement),
				};
			};
			const typed = () => {
				if (placeholder) placeholder.style.visibility = composerText() ? 'hidden' : '';
				record(composerState());
			};
			const owns = (event) =>
				container.contains(event.target) || [document.body, root, document, window].includes(event.target);
			const pointer = (event) => {
				if (!container.contains(event.target)) return;
				if (event.type === 'pointerdown' || event.type === 'mousedown') {
					pointerHeld = true;
					clearTimeout(pointerTimer);
				}
				if (event.type === 'pointerup' || event.type === 'mouseup' || event.type === 'pointercancel') {
					clearTimeout(pointerTimer);
					pointerTimer = later(() => {
						pointerHeld = false;
						if (!composing) finishComposition?.();
					}, 16);
				}
				event.stopImmediatePropagation();
				if (composer?.contains(event.target)) return;
				event.preventDefault();
				if (['click', 'auxclick', 'contextmenu', 'dblclick'].includes(event.type)) {
					record({
						kind: 'click',
						type: event.type,
						target: targetIdentity(event.target),
						x: event.clientX,
						y: event.clientY,
						button: event.button,
						detail: event.detail,
						...modifiers(event),
					});
					pointerHeld = false;
					clearTimeout(pointerTimer);
					if (!composing) finishComposition?.();
				}
			};
			const key = (event) => {
				if (!owns(event)) return;
				const inComposer = !!composer && composer.contains(event.target);
				const editing =
					inComposer &&
					!event.ctrlKey &&
					!event.metaKey &&
					!event.altKey &&
					(event.key.length === 1 ||
						['Backspace', 'Delete', 'Dead', 'Process'].includes(event.key) ||
						(event.key === 'Enter' && event.shiftKey));
				const commandModifiers = Number(event.ctrlKey) + Number(event.altKey) + Number(event.metaKey);
				const nativeEdit =
					inComposer &&
					((commandModifiers === 1 &&
						(event.ctrlKey || event.metaKey) &&
						['a', 'c', 'v', 'x', 'z', 'y'].includes(event.key.toLowerCase())) ||
						(commandModifiers <= 1 &&
							['ArrowLeft', 'ArrowRight', 'Backspace', 'Delete', 'Home', 'End'].includes(event.key)) ||
						(commandModifiers === 0 &&
							['ArrowUp', 'ArrowDown', 'PageUp', 'PageDown'].includes(event.key) &&
							(event.key !== 'ArrowUp' || event.shiftKey || composerText().length > 0)));
				if (composing || event.isComposing || editing || nativeEdit) {
					event.stopImmediatePropagation();
					return;
				}
				if (!inComposer && event.key.length === 1 && !event.ctrlKey && !event.metaKey && !event.altKey) {
					composer?.focus();
					event.stopImmediatePropagation();
					return;
				}
				const nativeScroll =
					!inComposer &&
					!event.ctrlKey &&
					!event.metaKey &&
					!event.altKey &&
					['ArrowUp', 'ArrowDown', 'PageUp', 'PageDown', 'Home', 'End'].includes(event.key);
				if (!nativeScroll) event.preventDefault();
				event.stopImmediatePropagation();
				if (inComposer && event.key === 'Enter' && !event.shiftKey) {
					if (event.type === 'keydown' && !event.repeat) {
						typed();
						record({kind: 'send', ...modifiers(event)});
					}
					return;
				}
				record({
					kind: 'key',
					type: event.type,
					key: event.key,
					code: event.code,
					keyCode: event.keyCode,
					location: event.location,
					repeat: event.repeat,
					target: targetIdentity(event.target),
					...modifiers(event),
				});
			};
			const scroll = (event) => {
				const element = event.target;
				if (!(element instanceof Element) || !container.contains(element)) return;
				const saved =
					positions.get(element) ||
					(element.getAttribute('{{BOOT_FRAME_SCROLL_ATTRIBUTE}}') || '0,0').split(',').map(Number);
				const top = element.scrollTop - saved[0];
				const left = element.scrollLeft - saved[1];
				positions.set(element, [element.scrollTop, element.scrollLeft]);
				if (top || left) record({kind: 'scroll', target: targetIdentity(element), top, left});
			};
			listen(window, 'keydown', key);
			listen(window, 'keyup', key);
			for (const type of [
				'pointerdown',
				'mousedown',
				'pointerup',
				'pointercancel',
				'mouseup',
				'click',
				'auxclick',
				'dblclick',
				'contextmenu',
			])
				listen(window, type, pointer);
			listen(document, 'scroll', scroll);
			if (composer) {
				listen(composer, 'input', typed);
				listen(composer, 'beforeinput', (event) => {
					if (
						event.inputType.startsWith('insert') &&
						(inputs.length >= Number('{{BOOT_FRAME_MAX_REPLAYED_INPUTS}}') - 2 ||
							composerText().length + (event.data?.length || event.dataTransfer?.getData('text/plain').length || 0) >
								initialLength + Number('{{BOOT_FRAME_MAX_REPLAYED_TEXT_LENGTH}}'))
					)
						event.preventDefault();
				});
				listen(document, 'selectionchange', () => {
					if (composer.contains(document.activeElement)) record(composerState());
				});
				listen(composer, 'compositionstart', () => {
					composing = true;
				});
				listen(composer, 'compositionend', () => {
					composing = false;
					typed();
					if (!pointerHeld) finishComposition?.();
				});
			}
			return {
				needsHistory: () => {
					const scroller = container.querySelector('[data-flx="channel.messages.scroller"]');
					if (!scroller) return false;
					const initial = Number((scroller.getAttribute('{{BOOT_FRAME_SCROLL_ATTRIBUTE}}') || '0,0').split(',')[0]);
					return scroller.scrollTop < initial;
				},
				stop: () => {
					for (const stop of listeners) stop();
					clearTimeout(compositionTimer);
					clearTimeout(pointerTimer);
					finishComposition?.();
					positions.clear();
				},
				whenInputSettled: () =>
					composing || pointerHeld
						? new Promise((resolve) => {
								finishComposition = () => {
									clearTimeout(compositionTimer);
									finishComposition = null;
									resolve();
								};
								compositionTimer = later(() => finishComposition?.(), 1000);
							})
						: Promise.resolve(),
				take: () => {
					for (const element of container.querySelectorAll('[data-flx]')) {
						if (
							element.scrollTop ||
							element.scrollLeft ||
							positions.has(element) ||
							element.hasAttribute('{{BOOT_FRAME_SCROLL_ATTRIBUTE}}')
						)
							scroll({target: element});
					}
					const finalComposer = composerState();
					const previousComposer = inputs.findLast((entry) => entry.kind === 'composer');
					if (
						(previousComposer || finalComposer.focused) &&
						JSON.stringify(previousComposer) !== JSON.stringify(finalComposer)
					)
						inputs.push(finalComposer);
					return {inputs, overflow};
				},
			};
		};
		const readDraft = async () => {
			const open = frameIndexedDB.open('{{APP_STORAGE_INDEXED_DB_NAME}}', Number('{{APP_STORAGE_INDEXED_DB_VERSION}}'));
			open.onupgradeneeded = () => open.transaction.abort();
			const db = await settle(open);
			try {
				const entries = db
					.transaction('{{APP_STORAGE_OBJECT_STORE}}', 'readonly')
					.objectStore('{{APP_STORAGE_OBJECT_STORE}}');
				const [record, streamerRecord] = await Promise.all([
					settle(entries.get(JSON.stringify([accountKey, '{{APP_STORAGE_DRAFTS_KEY}}']))),
					settle(entries.get(JSON.stringify(['{{GLOBAL_APP_STORAGE_SCOPE}}', '{{APP_STORAGE_STREAMER_MODE_KEY}}']))),
				]);
				const hidden = streamerRecord ? JSON.parse(streamerRecord.value)?.manualEnabled === true : false;
				const draft = record ? JSON.parse(record.value)?.drafts?.[location.pathname.split('/')[3]] : null;
				return {hidden, text: typeof draft === 'string' ? draft : ''};
			} finally {
				db.close();
			}
		};
		const showDraft = (container, draft) => {
			const composer = container.querySelector('{{BOOT_FRAME_TEXT_TARGET_SELECTOR}}');
			const line = composer?.firstElementChild;
			if (draft === null || !line) {
				return null;
			}
			composer.replaceChildren(
				...draft.split('\n').map((text) => {
					const paragraph = line.cloneNode(false);
					if (text) {
						const span = document.createElement('span');
						span.setAttribute('data-lexical-text', 'true');
						span.textContent = text;
						paragraph.append(span);
					} else {
						paragraph.append(document.createElement('br'));
					}
					return paragraph;
				}),
			);
			const placeholder = container.querySelector('{{BOOT_FRAME_COMPOSER_PLACEHOLDER_SELECTOR}}');
			if (placeholder) {
				placeholder.style.visibility = draft ? 'hidden' : '';
			}
			return draft.length > 0 ? composer : null;
		};
		const warmImages = async (sources) => {
			const embedded = [];
			const remote = [];
			for (const source of sources || []) {
				const image = new Image();
				image.src = source;
				const decoded = image.decode().catch(() => undefined);
				(source.startsWith('data:') ? embedded : remote).push(decoded);
			}
			await Promise.all([
				...embedded,
				Promise.race([
					Promise.all(remote),
					new Promise((resolve) => later(resolve, Number('{{BOOT_FRAME_IMAGE_WAIT_MS}}'))),
				]),
			]);
		};
		const showFrame = async (frame, draftRead) => {
			if (
				frameState.released ||
				frame.b !== build ||
				frame.p !== location.pathname ||
				innerWidth !== frame.w ||
				innerHeight !== frame.h ||
				devicePixelRatio !== frame.r ||
				(frame.d !== null && matchMedia('(prefers-color-scheme: dark)').matches !== frame.d) ||
				Array.from(root.classList).some((name) => name.startsWith('theme-') && !frame.c.includes(name))
			) {
				return;
			}
			const imagesWarmed = warmImages(frame.i);
			const sheets = new Map(
				Array.from(document.querySelectorAll('link[rel="stylesheet"]'), (link) => [link.getAttribute('href'), link]),
			);
			const embedded = [];
			const links = [];
			let anchor = null;
			const place = (node) => {
				if (anchor) {
					anchor.after(node);
				} else {
					document.head.appendChild(node);
				}
				anchor = node;
			};
			for (const sheet of frame.l) {
				if (sheet.text !== undefined) {
					if (sheet.text) {
						const inline = document.createElement('style');
						inline.textContent = sheet.text;
						cleanups.add(() => inline.remove());
						embedded.push(inline);
						place(inline);
					}
					continue;
				}
				let link = sheets.get(sheet.href);
				if (link) {
					anchor = link;
				} else {
					link = document.createElement('link');
					link.rel = 'stylesheet';
					link.crossOrigin = 'anonymous';
					if (sheet.integrity) link.integrity = sheet.integrity;
					link.href = sheet.href;
					place(link);
				}
				if (!link.sheet) links.push(loaded(link));
			}
			const style = document.createElement('style');
			style.textContent =
				'#{{BOOT_FRAME_ELEMENT_ID}} *,#{{BOOT_FRAME_ELEMENT_ID}} *::before,#{{BOOT_FRAME_ELEMENT_ID}} *::after{animation:none!important;transition:none!important}' +
				frame.t.join('\n');
			const fonts = Promise.all(links).then(() =>
				Promise.all(
					Array.from(document.fonts)
						.filter((face) =>
							frame.f.some(
								([family, weight, style]) => face.family === family && face.weight === weight && face.style === style,
							),
						)
						.map((face) => face.load()),
				),
			);
			const [appRoot, draft] = await Promise.race([
				Promise.all([whenRoot(), draftRead, imagesWarmed, fonts]),
				new Promise((_resolve, reject) => later(reject, Number('{{BOOT_FRAME_STYLESHEET_TIMEOUT_MS}}'))),
			]);
			if (frameState.released || !appRoot || draft.hidden) {
				for (const node of embedded) node.remove();
				return;
			}
			document.head.appendChild(style);
			const sheetWatcher = new MutationObserver((records) => {
				const added = records.some((record) =>
					Array.from(record.addedNodes).some((node) => node.tagName === 'LINK' && node.rel === 'stylesheet'),
				);
				if (added) {
					document.head.append(...embedded);
				}
			});
			sheetWatcher.observe(document.head, {childList: true});
			cleanups.add(() => sheetWatcher.disconnect());
			for (const name of frame.c) root.classList.add(name);
			const parsed = document.createElement('div').style;
			parsed.cssText = frame.s;
			for (const name of Array.from(parsed)) {
				if (!root.style.getPropertyValue(name)) {
					root.style.setProperty(name, parsed.getPropertyValue(name), parsed.getPropertyPriority(name));
				}
			}
			const container = document.createElement('div');
			container.id = '{{BOOT_FRAME_ELEMENT_ID}}';
			container.style.cssText = 'position:fixed;inset:0;z-index:2147483000;overflow:hidden';
			container.innerHTML = frame.x;
			const draftComposer = showDraft(container, draft.text);
			appRoot.after(container);
			for (const element of container.querySelectorAll('[{{BOOT_FRAME_SCROLL_ATTRIBUTE}}]')) {
				const [top, left] = element.getAttribute('{{BOOT_FRAME_SCROLL_ATTRIBUTE}}').split(',');
				element.scrollTop = Number(top);
				element.scrollLeft = Number(left);
				element.setAttribute('{{BOOT_FRAME_SCROLL_ATTRIBUTE}}', `${element.scrollTop},${element.scrollLeft}`);
			}
			const wasInert = appRoot.inert;
			cleanups.add(() => {
				appRoot.inert = wasInert;
			});
			appRoot.inert = true;
			const holding = holdInput(container);
			frameState.shown = {
				container,
				styles: [style, ...embedded],
				stopHolding: () => {
					sheetWatcher.disconnect();
					holding.stop();
					appRoot.inert = wasInert;
				},
				take: holding.take,
				whenInputSettled: holding.whenInputSettled,
				needsHistory: holding.needsHistory,
			};
			if (draftComposer) {
				draftComposer.focus({preventScroll: true});
				const selection = document.getSelection();
				if (selection) {
					const range = document.createRange();
					range.selectNodeContents(draftComposer);
					range.collapse(false);
					selection.removeAllRanges();
					selection.addRange(range);
				}
			}
			performance.mark('{{BOOT_FRAME_SHOWN_MARK}}');
			const paint = requestAnimationFrame(() => later(openGate, 0));
			cleanups.add(() => cancelAnimationFrame(paint));
		};
		const read = async () => {
			const digest = await crypto.subtle.digest(
				'SHA-256',
				encoder.encode(`{{BOOT_STATE_ACCOUNT_ID_PREFIX}}${accountKey}`),
			);
			const accountId = Array.from(new Uint8Array(digest, 0, Number('{{BOOT_STATE_ACCOUNT_ID_BYTES}}')), (byte) =>
				byte.toString(16).padStart(2, '0'),
			).join('');
			const open = frameIndexedDB.open('{{BOOT_STATE_DATABASE_NAME}}', Number('{{BOOT_STATE_DATABASE_VERSION}}'));
			open.onupgradeneeded = () => open.transaction.abort();
			const db = await settle(open);
			try {
				const transaction = db.transaction(['{{BOOT_STATE_KEY_STORE}}', '{{BOOT_STATE_STATE_STORE}}'], 'readonly');
				const [key, record] = await Promise.all([
					settle(transaction.objectStore('{{BOOT_STATE_KEY_STORE}}').get(accountId)),
					settle(transaction.objectStore('{{BOOT_STATE_STATE_STORE}}').get(accountId)),
				]);
				if (!key || !record || record.format !== format) {
					openGate();
					return {status: 'invalid'};
				}
				if (record.frame?.build === build) {
					const draftRead = readDraft().catch(() => ({hidden: true, text: ''}));
					inflate(key, record.frame, `{{BOOT_FRAME_AAD_PREFIX}}|${build}|${accountId}`)
						.then((bytes) => showFrame(JSON.parse(new TextDecoder().decode(bytes)), draftRead))
						.catch(removeFrame)
						.finally(() => {
							if (!frameState.shown) openGate();
						});
				} else {
					openGate();
				}
				const payload = await inflate(key, record, `{{BOOT_STATE_AAD_PREFIX}}|${format}|${accountId}`);
				return {status: 'hit', payload: new Uint8Array(payload), writtenAt: record.writtenAt};
			} finally {
				db.close();
			}
		};
		root.setAttribute('{{BOOT_STATE_EXPECTED_ATTRIBUTE}}', '{{BOOT_STATE_EXPECTED_VALUE}}');
		window['{{BOOT_STATE_PREBOOT_GLOBAL}}'] = {
			accountKey,
			result: read().catch(() => {
				removeFrame();
				openGate();
				return {status: 'invalid'};
			}),
		};
	} catch {}
})();
