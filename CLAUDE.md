# CLAUDE.md

이 저장소는 CRAN 배포 중인 패키지이며 **유지보수 전용**이다. 새
연구·기능 확장은 하지 않는다.

- 작업 전 maintenance/README.md의 워크플로와 최신 baseline 노트를
  읽는다.
- R/, src/, man/, tests/, DESCRIPTION, NAMESPACE 변경은 별도 브랜치 +
  PR로만. master 직접 push 금지.
- 변경 후 R CMD check 결과를 maintenance/ 노트에 기록한다.
- 배포(CRAN 제출), 가시성 변경, 버전 올리기는 사용자 명시 요청 시에만.
