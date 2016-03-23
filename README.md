[개인적으로 만든 아희-like 언어 설명 문서](https://gist.github.com/xnuk/1f6f65ce001c49703cdd)에 따라 구현된 인터프리터입니다.

## 설치 방법
[`stack install`](http://haskellstack.org) 하시면 `~/.local/bin`에 `another-aheui-interpreter` 실행 파일의 옥체가 보존되어 있으실 겁니다.

그게 싫으시면 [`stack build`](http://haskellstack.org) 하세요. 하시면

```
{프로젝트 폴더}/.stack-work/dist/{운영체제}/Cabal-{정말 멋진 버전 이름}/build/another-aheui-interpreter/another-aheui-interpreter
```

에 있을 겁니다.

## 중요한 것
### 작명
저 이름 짓는 거 못 합니다. 변수명도 대충 지었어요. 멋지죠.

## 별로 안 중요한 것
### 지원하는 `PASSAGE_SPEC` 목록
- [`NULL`](https://gist.github.com/xnuk/1f6f65ce001c49703cdd) ~ 0만 가득한 확장 공간 ~
- [`RANDOM`](https://gist.github.com/xnuk/259c130c5bb73086645b): 0 이상 32768 미만의 정수를 뱉습니다.

### 구현체가 지원하는 기타 플래그
- `DEBUGGER`: 실행 중 초성이 ㅇ이고 종성이 ㅎ인 한글 음절 문자를 만나면 `stderr`로 현재 상태를 출력합니다. 인터프리터를 빌드할 때의 컴파일 옵션에 따라 출력되지 않을 수 있습니다.
