#!/usr/bin/env python3
# SPDX-License-Identifier: AGPL-3.0-or-later

from __future__ import annotations

import argparse
import gzip
import json
import sys
from pathlib import Path

import brotli

sys.path.insert(0, str(Path(__file__).resolve().parent))

import build_fonts as bf  # noqa: E402

CACHE = bf.TOOL_ROOT / ".cache"
SFNT_CACHE = CACHE / "measure"

CORPUS: dict[str, dict[str, str]] = {
    "FluxerSansSC": {
        "one glyph (文)": "文",
        "display name": "小明",
        "three display names": "小明 李伟 王芳",
        "README sentence": "这是简体中文的示例文本",
        "chat message": "我们今天下午三点开个会吧，主要讨论新版本的发布计划和测试进度。",
        "chat paragraph": (
            "我们今天下午三点开个会吧，主要讨论新版本的发布计划和测试进度。"
            "如果有问题，可以随时在这个频道里留言，我会尽快回复大家。谢谢！"
        ),
        "UI labels": (
            "设置 通知 频道 消息 好友 语音 视频 屏幕共享 服务器 成员 权限 角色 邀请 退出 "
            "静音 耳机 麦克风 摄像头 主题 语言 隐私 安全 账号 个人资料 状态 在线 离线 忙碌 勿扰"
        ),
    },
    "FluxerSansTC": {
        "one glyph (文)": "文",
        "display name": "小明",
        "three display names": "小明 李偉 王芳",
        "README sentence": "這是繁體中文的示例文本",
        "chat message": "我們今天下午三點開個會吧，主要討論新版本的發佈計畫和測試進度。",
        "chat paragraph": (
            "我們今天下午三點開個會吧，主要討論新版本的發佈計畫和測試進度。"
            "如果有問題，可以隨時在這個頻道裡留言，我會盡快回覆大家。謝謝！"
        ),
        "UI labels": (
            "設定 通知 頻道 訊息 好友 語音 視訊 螢幕分享 伺服器 成員 權限 角色 邀請 離開 "
            "靜音 耳機 麥克風 攝影機 主題 語言 隱私 安全 帳號 個人資料 狀態 線上 離線 忙碌 勿擾"
        ),
    },
    "FluxerSansJP": {
        "one glyph (語)": "語",
        "display name": "田中太郎",
        "three display names": "田中太郎 佐藤花子 鈴木一郎",
        "README sentence": "これは日本語のサンプルテキストです",
        "chat message": "今日の午後三時から会議をしましょう。新しいバージョンの計画を話し合います。",
        "chat paragraph": (
            "今日の午後三時から会議をしましょう。主に新しいバージョンのリリース計画と"
            "テストの進捗について話し合います。質問があれば、いつでもこのチャンネルに"
            "書き込んでください。なるべく早く返信します。よろしくお願いします。"
        ),
        "UI labels": (
            "設定 通知 チャンネル メッセージ フレンド ボイス ビデオ 画面共有 サーバー メンバー "
            "権限 ロール 招待 退出 ミュート ヘッドセット マイク カメラ テーマ 言語 "
            "プライバシー セキュリティ アカウント プロフィール ステータス オンライン オフライン 取り込み中 応答不可"
        ),
    },
    "FluxerSansKR": {
        "one syllable (한)": "한",
        "display name": "김민준",
        "three display names": "김민준 이서연 박지훈",
        "README sentence": "이것은 한국어 샘플 텍스트입니다",
        "chat message": "오늘 오후 세 시에 회의를 합시다. 새 버전의 출시 계획을 이야기합니다.",
        "chat paragraph": (
            "오늘 오후 세 시에 회의를 합시다. 주로 새 버전의 출시 계획과 테스트 진행 상황에 "
            "대해 이야기할 예정입니다. 질문이 있으면 언제든지 이 채널에 남겨 주세요. "
            "최대한 빨리 답장하겠습니다. 감사합니다."
        ),
        "UI labels": (
            "설정 알림 채널 메시지 친구 음성 영상 화면 공유 서버 멤버 권한 역할 초대 나가기 "
            "음소거 헤드셋 마이크 카메라 테마 언어 개인정보 보안 계정 프로필 상태 온라인 오프라인 방해 금지"
        ),
    },
}

CJK_FAMILIES = tuple(CORPUS)


def renamed_reference(family: bf.Family, roots: dict[str, Path]) -> bytes:
    """The renamed Regular SFNT, cached: the WOFF2 round-trip costs ~60 s a face."""
    SFNT_CACHE.mkdir(parents=True, exist_ok=True)
    cached = SFNT_CACHE / f"{family.prefix}-Regular.sfnt"
    if cached.is_file():
        return cached.read_bytes()
    src = roots[family.source] / family.src_subdir / f"{family.prefix}-Regular.woff2"
    print(f"  renaming {family.prefix}-Regular (~60 s, cached afterwards)", flush=True)
    sfnt = bf.renamed_sfnt(src, family.gasp)
    cached.write_bytes(sfnt)
    return sfnt


def glyph_id_order(sfnt: bytes) -> list[int]:
    """This face's codepoints in glyph ID order (its inherited CID tiering)."""
    import io

    font = bf.TTFont(io.BytesIO(sfnt), lazy=True)
    try:
        cmap = font.getBestCmap()
        order = font.getGlyphOrder()
        rank = {name: index for index, name in enumerate(order)}
        return [cp for _, cp in sorted((rank[name], cp) for cp, name in cmap.items())]
    finally:
        font.close()


def plan_freq000(
    sfnt: bytes, codepoints: set[int], budget: int, head: int
) -> list[tuple[int, ...]]:
    """Chunk 000 = the `head` lowest-glyph-ID codepoints; the rest in block order."""
    ordered = [cp for cp in glyph_id_order(sfnt) if cp in codepoints]
    common = ordered[:head]
    if not common:
        raise SystemExit("empty head chunk")
    rest = codepoints - set(common)
    tail = bf.plan_chunks(rest, budget) if rest else []
    return [tuple(sorted(common)), *tail]


def stylesheet_bytes(family: bf.Family, plan: list[tuple[int, ...]]) -> dict[str, int]:
    """Render the real stylesheet for this plan and compress it as the app would.

    Chunk-major, every weight, `render_face_rule` verbatim — so the raw/gzip/
    brotli numbers are the ones a browser would actually download, not an
    estimate of the `unicode-range` text alone.
    """
    rules = [bf.SPDX_CSS_HEADER, bf.GENERATED_CSS_HEADER, ""]
    for index, chunk in enumerate(plan):
        rendered_range = bf.format_unicode_range(list(chunk))
        for weight in family.weights:
            rules.append(
                bf.render_face_rule(
                    family,
                    {
                        "weight": weight,
                        "path": f"{family.out_dir}/{family.prefix}-{weight}.{index:03d}.woff2",
                        "unicodeRange": rendered_range,
                    },
                )
            )
    text = ("\n".join([*rules, ""])).encode()
    return {
        "raw": len(text),
        "gzip": len(gzip.compress(text, 9, mtime=0)),
        "brotli": len(brotli.compress(text, quality=11)),
    }


def build_plan(family: bf.Family, sfnt: bytes, plan: list[tuple[int, ...]]) -> list[dict]:
    """Really subset and really encode every chunk, so the bytes are measured."""
    chunks = []
    for index, requested in enumerate(plan):
        produced = bf.subset_face(sfnt, tuple(requested))
        chunks.append({"index": index, "codepoints": set(requested), "bytes": len(produced)})
        print(f"    chunk {index:03d}: {len(requested)} cp -> {len(produced)} B", flush=True)
    return chunks


def fetched(chunks: list[dict], text: str) -> tuple[int, int]:
    """Bytes and chunk count a browser fetches to render `text` from this family.

    A chunk is fetched when its `unicode-range` intersects the text — which is
    exactly the browser's rule, and the reason spaces matter: U+0020 lives in the
    punctuation chunk, so any sentence containing one pays for it.
    """
    wanted = set(ord(c) for c in text)
    total = 0
    count = 0
    for chunk in chunks:
        if chunk["codepoints"] & wanted:
            total += chunk["bytes"]
            count += 1
    return total, count


def measure_family(family: bf.Family, roots: dict[str, Path], strategies: list[str]) -> dict:
    sfnt = renamed_reference(family, roots)
    union: set[int] = set()
    for name in family.source_files():
        union |= bf.face_cmap(roots[family.source] / family.src_subdir / name)
    budget, note = bf.calibrate_chunk_size(sfnt, sorted(union), bf.CHUNK_TARGET_BYTES)
    print(f"\n{family.out_dir}: {len(union)} codepoints, {note}")

    result: dict[str, dict] = {}
    for strategy in strategies:
        if strategy == "block":
            plan = bf.plan_chunks(union, budget)
        elif strategy.startswith("freq"):
            plan = plan_freq000(sfnt, union, budget, int(strategy[4:]))
        else:
            raise SystemExit(f"unknown strategy {strategy!r}")
        print(f"  {strategy}: {len(plan)} chunks", flush=True)
        chunks = build_plan(family, sfnt, plan)
        result[strategy] = {
            "chunks": len(plan),
            "css": stylesheet_bytes(family, plan),
            "full": sum(c["bytes"] for c in chunks),
            "scenarios": {
                label: fetched(chunks, text) for label, text in CORPUS[family.out_dir].items()
            },
        }
    return result


def report(family_id: str, data: dict[str, dict]) -> None:
    strategies = list(data)
    base = strategies[0]
    print(f"\n=== {family_id} ===")
    header = f"{'scenario':<24}" + "".join(f"{s:>22}" for s in strategies)
    print(header)
    print("-" * len(header))
    for label in CORPUS[family_id]:
        row = f"{label:<24}"
        base_bytes = data[base]["scenarios"][label][0]
        for strategy in strategies:
            got, count = data[strategy]["scenarios"][label]
            ratio = f" ({base_bytes / got:.2f}x)" if strategy != base and got else ""
            row += f"{f'{got:,} B/{count}c{ratio}':>22}"
        print(row)
    for key, label in (("full", "full coverage"), ("chunks", "chunk count")):
        row = f"{label:<24}"
        for strategy in strategies:
            value = data[strategy][key]
            row += f"{f'{value:,}':>22}"
        print(row)
    for field in ("raw", "gzip", "brotli"):
        row = f"{f'stylesheet {field}':<24}"
        for strategy in strategies:
            value = data[strategy]["css"][field]
            row += f"{f'{value:,} B':>22}"
        print(row)


def main() -> int:
    parser = argparse.ArgumentParser(description="Measure CJK chunking strategies.")
    parser.add_argument("--family", action="append", choices=CJK_FAMILIES)
    parser.add_argument("--strategies", default="block,freq1500,freq3000,freq6000")
    parser.add_argument("--json", type=Path, help="also write the raw numbers here")
    args = parser.parse_args()

    families = [bf.FAMILY_BY_DIR[f] for f in (args.family or CJK_FAMILIES)]
    strategies = [s.strip() for s in args.strategies.split(",") if s.strip()]
    roots = bf.resolve_sources(CACHE, tuple(families))

    everything: dict[str, dict] = {}
    for family in families:
        everything[family.out_dir] = measure_family(family, roots, strategies)
        report(family.out_dir, everything[family.out_dir])

    if args.json:
        args.json.write_text(
            json.dumps(
                {
                    fam: {
                        s: {
                            **{k: v for k, v in d.items() if k != "scenarios"},
                            "scenarios": {k: list(v) for k, v in d["scenarios"].items()},
                        }
                        for s, d in strategies_data.items()
                    }
                    for fam, strategies_data in everything.items()
                },
                indent="\t",
            )
            + "\n"
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
