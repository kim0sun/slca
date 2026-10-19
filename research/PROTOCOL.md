# PROTOCOL

이 저장소는 R 패키지이므로 아래 규칙의 모든 경로는 `research/` 기준으로 해석한다 (예: claude/ → research/claude/).


1. 연구 목적·구도·과제·최종 결정은 사용자가 정한다.
2. 매 실행 시작에 PROTOCOL.md와 shared/decisions.md, 해당 과제를 읽는다.
3. Claude는 claude/에, GPT는 gpt/에만 작성한다.
4. shared/는 사용자가 관리하며 두 에이전트는 읽기만 한다.
5. 상대 폴더는 읽을 수 있지만 수정하지 않는다.
6. 노트는 <agent>/<topic>/<YYYY-MM-DD>_<slug>.md에 저장한다.
7. 노트 머리에 topic, task, depends_on, status를 기록한다.
8. status는 draft, done, superseded를 쓰며 done은 해당 작업 완료를 뜻한다.
9. 핵심 주장에는 C1…과 유형(증명·수치·사실·해석)을 붙인다.
10. 수행 내용·근거·실제 검증 결과·한계·남은 일을 기록한다.
11. 상대 작업을 활용했다면 해당 파일과 commit을 출처로 남긴다.
12. 각자 INDEX.md에 날짜·제목·노트 링크·핵심 결론을 기록한다.
13. 완료 노트의 정정은 새 노트로 남기고 원래 노트와 연결한다.
14. main/master 직접 반영이 허용된 저장소에서는 자기 변경만 commit하며, push가 거절되면 원격 변경을 확인하고 타인의 작업을 덮어쓰지 않는다.
15. 교차검토는 사용자 요청 때 수행하고 자기 폴더에 기록한다.

---

노트 머리 형식:
```
---
topic: <topic>
task: shared/tasks/<file> 또는 한 줄 설명
depends_on: [<이전 노트 경로>, ...]
status: draft | done | superseded
---
```

INDEX.md 한 줄 형식:
```
- YYYY-MM-DD | <제목> | [<slug>](<topic>/<파일>) | <핵심 결론 한 줄>
```

수식은 amsmath 호환 LaTeX (Overleaf 컴파일 전제). 코드는 seed·패키지 버전 명시.
